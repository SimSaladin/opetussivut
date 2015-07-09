{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

------------------------------------------------------------------------------
-- | 
-- Module         : Main
-- Copyright      : (C) 2014-2015 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- 
-- This application is used to automatically generate web page bodies listing the available courses at the department of physics at the University of Helsinki. The data is parsed from a confluence Wiki Source Table containing the information needed to either directly generate the list or lookup more information from different web sites.
--
-- The application takes into consideration internationalization (I18N) for at least Finnish, Swedish and English, more languages might be supported in the future.
--
-- The user guide for operating the software can be found at <https://github.com/SimSaladin/opetussivut>
--
-- See config.yaml for configuration options.
--
------------------------------------------------------------------------------

module Main where

-- TODO: Remove the unused import?
-- import Prelude
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Concurrent.MVar
import           Codec.Text.IConv
import           Data.Char
import           Data.Function              (on)
import qualified Data.List          as L
import           Data.Map                   (Map)
import qualified Data.Map           as Map
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as LT
import qualified Data.Text.Lazy.IO  as LT
import           Data.Text.Lazy.Encoding as LT
import qualified Data.Yaml          as Yaml
import           Network.HTTP.Conduit       (simpleHttp)
import           Text.Blaze.Html            (preEscapedToHtml)
import           Text.Blaze.Renderer.Text   (renderMarkup)
import           Text.Hamlet
import           Text.Julius
import           Text.Regex
import qualified Text.XML           as XML
import           Text.XML.Cursor
import           Text.Markdown
import           Debug.Trace
import           Data.Time
import           System.Exit (exitFailure)
import           System.IO.Unsafe (unsafePerformIO) -- pure getCurrentTime
import           System.Environment (getArgs)
import           System.Directory
import           GHC.Generics


-- TODO: hard-coded level switch for "Taso" in categories configuration option
categoryLevelTaso :: Int
categoryLevelTaso = 1


{-| The entry point of the application.

    It reads the /config.yaml/ file into memory before doing anything else.
    
-}
main :: IO ()
main = Yaml.decodeFileEither "config.yaml" >>= either (error . show) (runReaderT go)
    where
  go = do
      Config{..} <- ask
      forM_ pages $ \pc@PageConf{..} -> do
          dt <- getData pageId
          case dt of
              Nothing -> liftIO $ putStrLn "Warning: failed to fetch doc, not updating listing"
              Just dt' -> do
                  table <- parseTable dt'
                  forM_ languages $ \lang -> renderTable rootDir lang pc table


-- ***************************************************************************
-- * Types
-- ***************************************************************************

-- | Short hand for the combination of the other three types.
type M = ReaderT Config IO

-- | The 'Lang' type is used as key for looking up the translations from the internationalization (I18N) data base found in the /config.yaml/ file.
type Lang = Text
-- ^ en, se, fi, ...

-- | Properties for generating individual web page bodies.
data PageConf = PageConf {  -- See Note [Config and PageConf] below
              -- Faculty webpage properties
                pageId          :: String
              , pageUrl         :: Map Lang Text
              , pageTitle       :: Map Lang Text
              } deriving Generic
instance Yaml.FromJSON PageConf

data Config   = Config {    -- See Note [Config and PageConf] below
              -- File handling properties
                rootDir         :: FilePath
              , cacheDir        :: FilePath
              , fetchUrl        :: String
              , weboodiUrl      :: Text
              , oodiNameFile    :: FilePath
              -- Page generation properties
              , languages       :: [Lang]
              , pages           :: [PageConf]
              -- Internationalization properties
              , i18n            :: I18N
              -- Wiki Table properties
              , categories      :: [[Text]]
              , colCode         :: Text
              , colLang         :: Text
              , colCourseName   :: Text
              , colRepeats      :: Text
              , colPeriod       :: Text
              , colWebsite      :: Text
              , colLukukausi    :: Text                 
              , classCur        :: Text
              } deriving Generic
instance Yaml.FromJSON Config

-- | Source table. Consists of a time stamp, a list of 'Header' objects and a list of 'Course' objects.
data Table        = Table UTCTime [Header] [Course]
                    deriving (Show, Read)

-- | Extension of 'Data.Text'. Used as Column headers when reading the source table.
type Header       = Text

-- TODO: Change the name of this type to fit its purpose more
-- | A row in source table.
type Course       = ([Category], Map Header ContentBlock)

-- | Extension of 'Data.Text'. First column in source table.
type Category     = Text

-- | \<td\> HTML tag in source table.
type ContentBlock = Text

-- | Internationalization database.
type I18N         = Map Text (Map Lang Text)


{- Note [Config and PageConf]
    
    = Config
    The 'Config' data type is used to read the configuration properties from
    the /config.yaml/ file. It holds references to all of the different property
    fields and are accessed by their name in the config.yaml file.
    
    = PageConf
    The 'PageConf' data type is used as a data holder for page information
    of the generated web pages. Where they are stored, what their titles are
    etc.
-}


-- ***************************************************************************
-- * Utility functions
-- ***************************************************************************


-- | Appends /.html/ to the end of the argument.
toUrlPath :: Text   -- ^ Argument: The URL without /.html/
          -> Text   -- ^ Return:   The URL with /.html/ at the end
toUrlPath  = (<> ".html")


-- | Prepends the argument with the value of the /root/ parameter, and appends /.body/ to the end of the argument.
--
-- > root <> arg <> .body
toFilePath :: FilePath -> Text -> FilePath
toFilePath root = (root <>) . T.unpack . (<> ".body")


-- | A hack, for confluence html is far from the (strictly) spec.
--
-- weboodi's html is just horrible (imo it's not even html), so we use
-- another regex to parse it (and not with the xml parser).
regexes :: [String -> String]
regexes = [ rm "<meta [^>]*>", rm "<link [^>]*>", rm "<link [^>]*\">", rm "<img [^>]*>"
          , rm "<br[^>]*>", rm "<col [^>]*>" ]
    where rm s i = subRegex (mkRegexWithOpts s False True) i ""

toLang :: I18N -> Lang -> Text -> Text
toLang db lang key = maybe (trace ("Warn: no i18n db for key `" ++ T.unpack key ++ "'") key)
                           (fromMaybe fallback . Map.lookup lang) (Map.lookup key db)
  where fallback | "fi" <- lang = key
                 | otherwise    = trace ("Warn: no i18n for key `" ++ T.unpack key ++ "' with lang `" ++ T.unpack lang ++ "'") key

lookup' :: Lang -> Map Lang y -> y
lookup' i = fromJust . Map.lookup i

normalize :: Text -> Text
normalize =
    T.dropAround (`elem` " ,-!")
    . T.replace "ILMOITTAUTUMINEN PUUTTUU" ""
    . T.unwords . map (T.unwords . T.words) . T.lines


-- ***************************************************************************
-- * Weboodi stuff
-- ***************************************************************************


getOodiName :: Text -> Maybe Text
getOodiName = fmap (T.pack
                   . sub "&aring;" "å"
                   . sub "&auml;" "ä"
                   . sub "&Aring;" "Å"
                   . sub "&Auml;" "Ä"
                   . sub "&ouml;" "ö"
                   . sub "&Ouml;" "Å"
                   . sub "&#x3a;" ":"
                   . sub "&#x27;" "'"
                   . sub "&#x28;" "("
                   . sub "&#x29;" ")"
                   . sub "&#x3b;" ";"
                   . head)
            . matchRegex (mkRegexWithOpts s True False) . T.unpack
    where s = "tauluotsikko\"?>[0-9 ]*(.*),[^,]*<"
          sub a b i = subRegex (mkRegexWithOpts a False True) i b

weboodiLang :: Lang -> Text
weboodiLang "fi" = "1"
weboodiLang "se" = "2"
weboodiLang "en" = "6"
weboodiLang _    = ""

weboodiLink :: Text -> Lang -> Text -> Text
weboodiLink url lang pid = url <> weboodiLang lang <> "&Tunniste=" <> pid

oodiVar :: MVar (Map (Lang, Text) Text)
oodiVar = unsafePerformIO newEmptyMVar
{-# NOINLINE oodiVar #-}
     
readOodiNames :: M (Map (Lang, Text) Text)
readOodiNames = do
    Config{..} <- ask
    exists <- liftIO $ doesFileExist oodiNameFile
    if exists
        then liftIO $ read <$> readFile oodiNameFile
        else return Map.empty

i18nCourseNameFromOodi :: Lang -> Text -> M (Maybe Text)
i18nCourseNameFromOodi lang pid = do
    Config{..} <- ask

    ov <- liftIO $ tryTakeMVar oodiVar
    oodiNames <- case ov of
        Just x  -> return x
        Nothing -> readOodiNames
    liftIO $ putMVar oodiVar oodiNames

    case Map.lookup (lang, pid) oodiNames of
        Just name -> return $ Just name
        Nothing   -> do
            raw <- liftIO $ fetch8859 (T.unpack $ weboodiLink weboodiUrl lang pid)
            case getOodiName raw of
                Nothing   -> return Nothing
                Just name -> do
                    let newNames = Map.insert (lang, pid) name oodiNames
                    liftIO $ do _ <- swapMVar oodiVar newNames
                                writeFile oodiNameFile (show newNames)
                    return $ Just name


-- ***************************************************************************
-- * Rendering
-- ***************************************************************************


renderTable :: FilePath -> Lang -> PageConf -> Table -> M ()
renderTable root lang pc@PageConf{..} table =
    ask >>= lift . LT.writeFile fp . renderMarkup . tableBody lang pc table
  where fp = toFilePath root $ lookup' lang pageUrl


-- ***************************************************************************
-- * Content
-- ***************************************************************************

-- | How to render the data
tableBody :: Lang -> PageConf -> Table -> Config -> Html
tableBody lang page (Table time _ stuff) cnf@Config{..} =
        let ii                       = toLang i18n lang
            translateCourseName code = unsafePerformIO $
                runReaderT (i18nCourseNameFromOodi lang code) cnf

-- course category div -------------------------------------------------
            withCat n xs f = [shamlet|
$forall ys <- L.groupBy (catGroup cnf n) xs
    <div.courses>
        #{ppCat n ys}
        #{f ys}
|]
-- course table --------------------------------------------------------
            go n xs
                | n == length categories = [shamlet|
<table style="width:100%">
 $forall c <- xs
  <tr data-taso="#{fromMaybe "" $ catAt cnf categoryLevelTaso c}" data-kieli="#{getThing colLang c}" data-lukukausi="#{getThing colLukukausi c}" data-pidetaan="#{getThing "pidetään" c}">
    <td style="width:10%">
      <a href="#{weboodiLink weboodiUrl lang $ getThing colCode c}">
        <b>#{getThing colCode c}

    <td style="width:55%">

      $maybe name <- translateCourseName (getThing colCode c)
        #{name}
      $nothing
        #{getThing colCourseName c}

      $with op <- getThing "op" c
          $if not (T.null op)
            \ (#{op} #{ii "op"})

    <td.compact style="width:7%"  title="#{getThing colPeriod c}">#{getThing colPeriod c}
    <td.compact style="width:7%"  title="#{getThing colRepeats c}">#{getThing colRepeats c}
    <td.compact style="width:8%;font-family:monospace" title="#{getThing colLang c}">
      $case T.words (getThing colLang c)
        $of []
        $of xs
            <b>#{head xs}
            $if null $ tail xs
            $else
                (#{T.intercalate ", " $ tail xs})

    <td.compact style="width:12%" title="#{colWebsite}">
      $maybe p <- getThingMaybe colWebsite c
        $if not (T.null p)
            \ #
            <a href="#{p}">#{ii colWebsite}
|]
                | otherwise = withCat n xs (go (n + 1))

-- if it begins with a number, apply appropriate header ---------------
            ppCat n xs     = [shamlet|
$maybe x <- catAt cnf n (head xs)
    $case n
        $of 0
            <h1>#{ii x}
        $of 1
            <h2>#{ii x}
        $of 2
            <h3>
                <i>#{ii x}
        $of 3
            <h4>#{ii x}
        $of 4
            <h5>#{ii x}
        $of 5
            <h6>#{ii x}
        $of _
            <b>#{ii x}
            |]

-- put everything together --------------------------------------------
        in [shamlet|
\<!-- title: #{lookup' lang $ pageTitle page} -->
\<!-- fi (Suomenkielinen versio): #{toUrlPath $ lookup' "fi" $ pageUrl page} -->
\<!-- se (Svensk version): #{toUrlPath $ lookup' "se" $ pageUrl page} -->
\<!-- en (English version): #{toUrlPath $ lookup' "en" $ pageUrl page} -->
\ 

\<!-- !!! IMPORTANT !!! -->
\<!-- THIS PAGE IS GENERATED AUTOMATICALLY -- DO NOT EDIT DIRECTLY! -->
\<!-- See https://github.com/SimSaladin/opetussivut instead -->

<p>
  $with pg <- head pages
    <a href="#{toUrlPath $ fromJust $ Map.lookup lang $ pageUrl pg}">#{fromJust $ Map.lookup lang $ pageTitle pg}
  $forall pg <- tail pages
    \ | 
    <a href="#{toUrlPath $ fromJust $ Map.lookup lang $ pageUrl pg}">#{fromJust $ Map.lookup lang $ pageTitle pg}

<p>
  #{markdown def $ LT.fromStrict $ ii "aputeksti"}

<p>
  #{ii "Kieli"}:&nbsp;
  <select id="select-kieli" name="kieli" onchange="updateList(this)">
     <option value="any">#{ii "Kaikki"}
     $forall l <- languages
        <option value="#{l}">#{ii l}

  #{ii "Taso"}:&nbsp;
  <select id="select-taso" name="taso" onchange="updateList(this)">
     <option value="any" >#{ii "Kaikki"}
     $forall cat <- (categories !! categoryLevelTaso)
        <option value="#{cat}">#{ii cat}

  #{ii "Lukukausi"}:&nbsp;
  <select id="select-lukukausi" name="lukukausi" onchange="updateList(this)">
     <option value="any"   >#{ii "Kaikki"}
     <option value="kevät" >#{ii "Kevät"}
     <option value="syksy" >#{ii "Syksy"}
     <option value="kesä"  >#{ii "Kesä"}

<table style="width:100%">
    <tr>
        <td style="padding-left:0.5em;width:10%">#{ii colCode}
        <td style="padding-left:0.5em;width:55%">#{ii colCourseName}
        <td style="padding-left:0.5em;width:7%" >#{ii colPeriod}
        <td style="padding-left:0.5em;width:7%" >#{ii colRepeats}
        <td style="padding-left:0.5em;width:8%" >#{ii colLang}
        <td style="padding-left:0.5em;width:12%">#{ii colWebsite}

#{withCat 0 stuff (go 1)}

<p>#{ii "Päivitetty"} #{show time}
<style>
    .courses table { table-layout:fixed; }
    .courses td.compact {
        overflow:hidden;
        text-overflow:ellipsis;
        white-space:nowrap;
    }
    tr[data-pidetaan="next-year"] { color:gray; }
<script type="text/javascript">
  #{preEscapedToHtml $ renderJavascript $ jsLogic undefined}
|]

--
jsLogic :: JavascriptUrl url
jsLogic = [julius|

fs = { };

updateList = function(e) {
    var name = e.getAttribute("name");
    var opts = e.selectedOptions;

    fs[name] = [];
    for (var i = 0; i < opts.length; i++) {
        fs[name].push(opts[i].getAttribute("value"));
    }

    var xs = document.querySelectorAll(".courses tr");

    for (var i = 0; i < xs.length; i++) {
        xs[i].hidden = !matchesFilters(fs, xs[i]);
    }

    updateHiddenDivs();
}

matchesFilters = function(fs, thing) {
    for (var f in fs) {
        if (fs[f] != "any") {
            var m = false;
            for (var i = 0; i < fs[f].length; i++) {
                if (thing.dataset[f].indexOf(fs[f][i]) > -1) {
                    m = true;
                    break;
                }
            }
            if (!m) return false;
        }
    }
    return true;
}

updateHiddenDivs = function() {
    var xs = document.querySelectorAll(".courses");
    for (var i = 0; i < xs.length; i++) {
        var hidden = true;
        var ts = xs[i].getElementsByTagName("tr");
        for (var j = 0; j < ts.length; j++) {
            if (!ts[j].hidden) {
                hidden = false;
                break;
            }
        }
        xs[i].hidden = hidden;
    }
}
|]


-- ***************************************************************************
-- * Courses and categories
-- ***************************************************************************


toCourse :: Config -> [Category] -> [Header] -> Bool -> [Text] -> Course
toCourse Config{..} cats hs iscur xs =
    (cats, Map.adjust doLang colLang $
           Map.adjust doRepeats colRepeats $
           Map.insert "pidetään" (if iscur then "this-year" else "next-year") $
           Map.insert colLukukausi lukukausi vals)
  where vals      = Map.fromList $ zip hs $ map normalize xs
        lukukausi = fromMaybe "syksy, kevät" $ Map.lookup colPeriod vals >>= toLukukausi
        toLukukausi x
            | x == "I"   || x == "II" || x == "I-II"   = Just "syksy"
            | x == "III" || x == "IV" || x == "III-IV" = Just "kevät"
            | x == "V"                                 = Just "kesä"
            | x == "I-IV"                              = Just "syksy, kevät"
            | "kevät" `T.isInfixOf` x                  = Just "kevät"
            | "syksy" `T.isInfixOf` x                  = Just "syksy"
            | "kesä"  `T.isInfixOf` x                  = Just "kesä"
            | otherwise                                = Nothing

doRepeats :: Text -> Text
doRepeats x | T.any isLetter x = "-"
            | otherwise        = x

doLang :: Text -> Text
doLang = T.replace "suomi" "fi" . T.replace "eng" "en" . T.replace "englanti" "en"
       . T.replace "ruotsi" "se"
       . T.unwords . T.words
       . T.replace "," " " . T.replace "." " " . T.replace "/" " " . T.toLower

-- | Accumulate a category to list of categories based on what categories
-- cannot overlap
accumCategory :: Config -> Category -> [Category] -> [Category]
accumCategory Config{..} c cs = case L.findIndex (any (`T.isPrefixOf` c)) categories of
    Nothing -> error $ "Unknown category: " ++ show c
    Just i  -> L.deleteFirstsBy T.isPrefixOf cs (f i) ++ [c]
    where f i = concat $ L.drop i categories

toCategory :: Config -> Text -> Maybe Category
toCategory Config{..} t = do
    guard $ t /= "\160" && t /= "syksy" && t /= "kevät"
    guard $ isJust $ L.find (`T.isInfixOf` t) $ concat categories
    return $ normalize t

catAt :: Config -> Int -> Course -> Maybe Text
catAt Config{..} n (cats, _) = case [ c | c <- cats, cr <- categories !! n, cr `T.isPrefixOf` c ] of
                                   x:_ -> Just x
                                   _   -> Nothing

catGroup :: Config -> Int -> Course -> Course -> Bool
catGroup cnf n = (==) `on` catAt cnf n

getThing :: Text -> Course -> Text
getThing k c = fromMaybe (traceShow ("Key not found" :: String, k, c) $ "Key not found: " <> k) $ getThingMaybe k c

getThingMaybe :: Text -> Course -> Maybe Text
getThingMaybe k (_, c) = Map.lookup k c

getThingLang :: I18N -> Lang -> Text -> Course -> Text
getThingLang db lang key c = fromMaybe (getThing key c) $ getThingMaybe (toLang db lang key) c


-- ***************************************************************************
-- * Get source
-- ***************************************************************************


parseSettings :: XML.ParseSettings
parseSettings = XML.def { XML.psDecodeEntities = XML.decodeHtmlEntities }

-- | Fetch a confluence doc by id.
getData :: String -> M (Maybe XML.Document)
getData pid = do
    Config{..} <- ask
    xs         <- lift getArgs
    let file   = cacheDir <> "/" <> pid <> ".html"

    str <- lift $ case xs of
        "cache" : _ -> Just <$> LT.readFile file
        "fetch" : _ -> do liftIO . putStrLn $ "Fetching doc id " <> show pid
                          r <- LT.decodeUtf8 <$> simpleHttp (fetchUrl ++ pid)
                          if "<title>Log In" `LT.isInfixOf` r
                              then return Nothing
                              else LT.writeFile file r >> return (Just r)
        _           -> putStrLn "Usage: opetussivut <fetch|cache>" >> exitFailure

    return $ cleanAndParse <$> str

cleanAndParse :: LT.Text -> XML.Document
cleanAndParse = XML.parseText_ parseSettings . LT.pack . foldl1 (.) regexes . LT.unpack

fetch8859 :: String -> IO Text
fetch8859 url = LT.toStrict . LT.decodeUtf8 . convert "iso-8859-1" "utf-8" <$> simpleHttp url


-- ***************************************************************************
-- * Parse doc
-- ***************************************************************************


parseTable :: XML.Document -> M Table
parseTable doc = head . catMaybes . findTable (fromDocument doc) <$> ask

findTable :: Cursor -> Config -> [Maybe Table]
findTable c cnf = map ($| processTable cnf) (c $.// attributeIs "class" "confluenceTable" :: [Cursor])

getHeader :: Cursor -> Maybe Header
getHeader c = return x <* guard (not $ T.null x)
  where x = T.toLower . normalize $ T.unwords (c $// content)

processTable :: Config -> Cursor -> Maybe Table
processTable cnf c = case cells of
    _ : header : xs ->
        let headers       = mapMaybe getHeader header
            (_, mcourses) = L.mapAccumL (getRow cnf headers) [] xs
        in Just $ Table (unsafePerformIO getCurrentTime) headers (catMaybes mcourses)
    _               -> Nothing
  where
    cells = map ($/ anyElement) (c $// element "tr")

-- | A row is either a category or course. The @[Category]@ is used as an
-- accumulator.
getRow :: Config -> [Header] -> [Category] -> [Cursor] -> ([Category], Maybe Course)
getRow cnf@Config{..} hs cats cs = map (T.unwords . ($// content)) cs `go` head (cs !! 1 $| attribute "class")
    where go []        _       = (cats, Nothing)
          go (mc : vs) classes = case toCategory cnf mc of
                Just cat                        -> (accumCategory cnf cat cats, Nothing)
                Nothing | null vs               -> (cats, Nothing)
                        | T.null (normalize mc) -> (cats, Just $ toCourse cnf cats hs (classCur `T.isInfixOf` classes) vs)
                        | otherwise             -> (cats, Just $ toCourse cnf cats hs (classCur `T.isInfixOf` classes) vs)
