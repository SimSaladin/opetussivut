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
-- This application is used to automatically generate web page bodies listing
-- the available courses at the department of physics at the University of Helsinki.
-- The data is parsed from a confluence Wiki Source Table containing the information
-- needed to either directly generate the list or lookup more information from
-- different web sites.
--
-- The application takes into consideration internationalization (I18N) for at
-- least Finnish, Swedish and English, more languages might be supported in the future.
--
-- The user guide for operating the software can be found at <https://github.com/SimSaladin/opetussivut>
--
-- See config.yaml for configuration options.
--
------------------------------------------------------------------------------

module Main where

-- TODO: Remove the unused import?
-- TODO: Fix the order to follow more standard ones!
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


-- | The entry point of the application.
--
-- It reads the /config.yaml/ file into memory before doing anything else.
main :: IO ()
main = trace ("============================================================\n" ++
              " Running Application\n" ++
              "============================================================\n" ++
              " * Decoding `config.yaml'")
    Yaml.decodeFileEither "config.yaml" >>= either (error . show) (runReaderT go)
  where
    go = do
        Config{..} <- ask
        forM_ pages $ \pc@PageConf{..} -> do
            pageData <- 
                trace (" * Reading data from Wiki page ID: " ++ pageId)
                getData pageId
            case pageData of
                Nothing -> liftIO $ putStrLn "!!! Warning: failed to fetch doc, not updating the listings..."
                Just pageData' -> do
                    table <- parseTable pageData'
                    forM_ languages $ \lang -> 
                        trace (" *     Language: " ++ show lang)
                        renderTable rootDir lang pc table


-- ===========================================================================
-- * Types
-- ===========================================================================


-- TODO: Why is it using the three types, ReaderT Config IO
-- | Short hand for the combination of the three types.
type M = ReaderT Config IO


-- | The 'Lang' type is used as key for looking up the translations from the
-- internationalization ('I18N') database found in the /config.yaml/ file.
--
-- It is used with acronyms of the languages: @fi@, @se@, @en@, ...
type Lang = Text


-- | Properties for generating individual web page bodies.
data PageConf = PageConf {  -- See Note [Config and PageConf] below
              -- Faculty webpage properties
                pageId          :: String
              , pageUrl         :: Map Lang Text
              , pageTitle       :: Map Lang Text
              } deriving Generic
instance Yaml.FromJSON PageConf


-- | Overall properties used by the module to generate HTML code from the
-- source 'Table' found in the wiki pages.
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


-- | Source 'Table'. Consists of a time stamp, a list of 'Header' objects and a list of 'Course' objects.
data Table        = Table UTCTime [Header] [Course]
                    deriving (Show, Read)


-- | Extension of 'Data.Text'. Used as Column 'Header's when reading the source 'Table'.
type Header       = Text


-- TODO: Change the name of this type to fit its purpose more
-- | A row in source 'Table'. Each cell of the 'Table' row is mapped to a 'Header', and
-- each cell can have multiple 'Category's coupled to it (see 'Category').
type Course       = ([Category], Map Header ContentBlock)


-- | Extension of 'Data.Text'. First column in source 'Table'. Each 'Course' can have
-- several 'Category's (eg. @Pääaineopinnot@, @Perusopinnot@, @Pakolliset@, etc).
type Category     = Text


-- | @\<td\>@ HTML tag in source 'Table'.
type ContentBlock = Text


-- | Internationalization database.
--
-- Map of a Finnish 'Text' phrase (fragment) to a list of 'Text's mapped to 'Lang'uages.
-- This way it is easy to translate a specific Finnish phrase to one of the supported
-- languages.
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


-- ===========================================================================
-- * Utility functions
-- ===========================================================================


-- | Appends /.html/ to the end of the argument.
toUrlPath :: Text   -- ^ Argument: The URL without /.html/
          -> Text   -- ^ Return:   The URL with /.html/ at the end
toUrlPath  = (<> ".html")


-- | Prepends the argument with the value of the /root/ parameter,
-- and appends /.body/ to the end of the argument.
--
-- > root <> arg <> .body
--
-- The PHP-engine used on physics.helsinki.fi utilizes a own .html file
-- extension (.body) to show HTML output in the center of the page.
toFilePath :: FilePath  -- ^ Argument: The root directory.
           -> Text      -- ^ Argument: The file name.
           -> FilePath  -- ^ Return:   @root/filename.body@
toFilePath root = (root <>) . T.unpack . (<> ".body")


-- | A hack, because confluence HTML is far from the (strictly) spec.
--
-- WebOodi's HTML is just horrible (imo it's not even HTML), so we use
-- another regex to parse it (before the XML parser is used).
regexes :: [String -> String]
regexes = [ rm "<meta [^>]*>", rm "<link [^>]*>", rm "<link [^>]*\">", rm "<img [^>]*>"
          , rm "<br[^>]*>", rm "<col [^>]*>" ]
    where rm s i = subRegex (mkRegexWithOpts s False True) i ""


-- | Fetch a translation from the 'I18N' database, /db/, found in /config.yaml/.
--
-- If the 'Text' that is to be translated can't be found in the given database.
-- It'll fall back on the given 'Text' (the Finnish version). If the selected
-- 'Lang'uage in this case is not Finnish it will also warn the user that no
-- translation for the selected 'Text' can be found.
toLang :: I18N  -- ^ Argument: The 'I18N' database to fetch translations from.
       -> Lang  -- ^ Argument: The 'Lang'uage to fetch translation for.
       -> Text  -- ^ Argument: The 'Text' to translate.
       -> Text  -- ^ Return:   The translated 'Text' or the 'Text' to translate if no translation was found.
toLang db lang key = maybe (trace ("!!! Warning: no i18n db for key `" ++ T.unpack key ++ "'") key)
                           (fromMaybe fallback . Map.lookup lang) (Map.lookup key db)
  where fallback | "fi" <- lang = key
                 | otherwise    = trace ("!!! Warning: no i18n for key `" ++ T.unpack key ++ "' with lang `" ++ T.unpack lang ++ "'") key


-- | Lookup a translation from any given map, containing 'y' types
-- mapped to 'Lang' types. It only returns 'Just' values of the result.
lookupLang :: Lang         -- ^ Argument: The 'Lang'uage to look for.
        -> Map Lang y   -- ^ Argument: A map to fetch type 'y' from.
        -> y            -- ^ Return:   The value of type 'y' found in the map.
lookupLang i = fromJust . Map.lookup i


-- TODO: Check what this actually does.
-- TODO: Why is the T.words and T.unwords called directly after each other?
-- | 
normalize :: Text   -- ^ Argument: The 'Text' to normalize.
          -> Text   -- ^ Result:   The normalized 'Text'.
normalize =
    T.dropAround (`elem` " ,-!")
    . T.replace "ILMOITTAUTUMINEN PUUTTUU" ""
    . T.unwords . map (T.unwords . T.words) . T.lines


-- ===========================================================================
-- * Weboodi stuff
-- ===========================================================================


-- | Fetch the course name from the WebOodi URL. It looks for a HTML tag containing
-- the text /tauluotsikko\"?>/ to find the name of the course. It then replaces some
-- sub strings in the course name with Unicode characters.
getOodiName :: Text         -- ^ Argument: 'Text' containing a raw version of the course name.
            -> Maybe Text   -- ^ Return:   The Unicode formatted version of the course name.
getOodiName =
    fmap (T.pack
          . sub "&aring;" "å"
          . sub "&Aring;" "Å"
          . sub "&auml;" "ä"
          . sub "&Auml;" "Ä"
          . sub "&ouml;" "ö"
          . sub "&Ouml;" "Ö"
          . sub "&#x3a;" ":"
          . sub "&#x27;" "'"
          . sub "&#x28;" "("
          . sub "&#x29;" ")"
          . sub "&#x3b;" ";"
          . head) . matchRegex (mkRegexWithOpts s True False) . T.unpack
  where s         = "tauluotsikko\"?>[0-9 ]*(.*),[^,]*<"
        sub a b i = subRegex (mkRegexWithOpts a False True) i b


-- | Helper method to select the correct 'Lang'uage when using WebOodi.
-- The 'Lang'uage is changed by switching a number in the URL:
--
--      * 1 : fi (Finnish version)
--
--      * 2 : se (Swedish version)
--
--      * 6 : en (English version)
--
-- If another 'Lang'uage than the provided ones is used, the function
-- returns an empty 'string' instead of a number.
weboodiLang :: Lang     -- ^ Argument: The 'Lang' to use on WebOodi.
            -> Text     -- ^ Return:   A 'Text' string consisting of the number to switch to in the WebOodi URL.
weboodiLang lang
            | "fi" <- lang = "1"
            | "se" <- lang = "2"
            | "en" <- lang = "6"
            | otherwise    = ""


-- TODO: Change the example to use the real base URL found in the /config.yaml/ file
-- | Creates a hyperlink to use for accessing the selected 'Lang'uage version of WebOodi.
--
-- The return value has the form of: /[base URL][1/2/6]"&Tunniste="[page ID]/.
weboodiLink :: Text     -- ^ Argument: The base URL of WebOodi.
            -> Lang     -- ^ Argument: Language to use on WebOodi.
            -> Text     -- ^ Argument: WebOodi page ID.
            -> Text     -- ^ Return:   The concatenated URL.
weboodiLink url lang pageId = url <> weboodiLang lang <> "&Tunniste=" <> pageId


-- TODO: What does it do?
oodiVar :: MVar (Map (Lang, Text) Text)
oodiVar = unsafePerformIO newEmptyMVar
{-# NOINLINE oodiVar #-}


-- | Read the course names from the /oodiNameFile/.
--
-- If the file exists this function returns a map of translations for the different
-- course names, otherwise it'll return an empty map.
readOodiNames :: M (Map (Lang, Text) Text)
readOodiNames = do
    Config{..} <- ask
    exists <- liftIO $ doesFileExist oodiNameFile
    if exists
        then liftIO $ read <$> readFile oodiNameFile
        else return Map.empty


-- TODO: Check what this method does, step by step.
-- | Get a translated name for a specific course, by looking it up from WebOodi.
--
--
i18nCourseNameFromOodi :: Lang              -- ^ Argument: The 'Lang'uage to lookup.
                       -> Text              -- ^ Argument: The WebOodi page ID.
                       -> M (Maybe Text)    -- ^ Return:   The translation if found.
i18nCourseNameFromOodi lang pageId = do
    Config{..} <- ask

    ov         <- liftIO $ tryTakeMVar oodiVar
    oodiNames  <- case ov of
        Just x  -> return x
        Nothing -> readOodiNames
    liftIO $ putMVar oodiVar oodiNames

    case Map.lookup (lang, pageId) oodiNames of
        Just name -> return $ Just name
        Nothing   -> do
            raw <- liftIO $ fetch8859 (T.unpack $ weboodiLink weboodiUrl lang pageId)
            case getOodiName raw of
                Nothing   -> return Nothing
                Just name -> do
                    let newNames = Map.insert (lang, pageId) name oodiNames
                    liftIO $ do _ <- swapMVar oodiVar newNames
                                writeFile oodiNameFile (show newNames)
                    return $ Just name


-- | Reads the web page at the given URL and returns a utf-8 converted version of the
-- page. WebOodi is encoded in iso-8859-1 encoding.
fetch8859 :: String     -- ^ Argument: URL with iso-8859-1 encoding.
          -> IO Text    -- ^ Return:   The web page converted to utf-8.
fetch8859 url = LT.toStrict . LT.decodeUtf8 . convert "iso-8859-1" "utf-8" <$> simpleHttp url


-- ===========================================================================
-- * Rendering
-- ===========================================================================


-- | Create the final HTML version of the web page from the cached one.
renderTable :: FilePath     -- ^ Argument: The root to where the HTML file should be saved.
            -> Lang         -- ^ Argument: The currently used language.
            -> PageConf     -- ^ Argument: More specific information of the current web page being created.
            -> Table        -- ^ Argument: The source table to use when creating the web page
            -> M ()         -- ^ Return:   ReaderT Config IO, from the generated HTML file.
renderTable root lang pc@PageConf{..} table =
    ask >>= lift . LT.writeFile fp . renderMarkup . tableBody lang pc table
  where fp = toFilePath root $ lookupLang lang pageUrl


-- ===========================================================================
-- * Content
-- ===========================================================================


-- | How to render the data of the selected table into HTML using WebOodi
-- to lookup course names in different languages.
tableBody :: Lang           -- ^ Argument: The currently used 'Lang'uage.
          -> PageConf       -- ^ Argument: More specific information of the current web page being created.
          -> Table          -- ^ Argument: The source table to use when creating the web page.
          -> Config         -- ^ Argument: Configuration containing specific information about the source table and translation data.
          -> Html           -- ^ Return:   The generated HTML code.
tableBody lang page (Table time _ tableContent) cnf@Config{..} =
    let i18nTranslationOf        = toLang i18n lang
        translateCourseName code = unsafePerformIO $
            runReaderT (i18nCourseNameFromOodi lang code) cnf

        -- course table --------------------------------------------------------
        courseTable n rows
            | n < length categories = withCat n rows $ courseTable (n + 1)
            | otherwise             =
                [shamlet|
                    <table>
                        $forall c <- rows
                            <tr data-taso="#{fromMaybe "" $ catAt cnf categoryLevelTaso c}"
                                data-kieli="#{getCellContent colLang c}"
                                data-lukukausi="#{getCellContent colLukukausi c}"
                                data-pidetaan="#{getCellContent "pidetään" c}">

                                <td style="width:10%">
                                    <a href="#{weboodiLink weboodiUrl lang $ getCellContent colCode c}">
                                        <b>#{getCellContent colCode c}

                                <td style="width:57%">

                                    $maybe name <- translateCourseName (getCellContent colCode c)
                                        #{name}
                                    $nothing
                                        #{getCellContent colCourseName c}

                                    $with op <- getCellContent "op" c
                                        $if not (T.null op)
                                            \ (#{op} #{i18nTranslationOf "op"})

                                <td.compact style="width:8%"  title="#{getCellContent colPeriod c}">#{getCellContent colPeriod c}
                                <td.compact style="width:8%"  title="#{getCellContent colRepeats c}">#{getCellContent colRepeats c}
                                <td.compact style="width:12%;font-family:monospace" title="#{getCellContent colLang c}">
                                    $case T.words (getCellContent colLang c)
                                        $of []
                                        $of rows
                                            <b>#{head rows}
                                            $if null $ tail rows
                                            $else
                                                (#{T.intercalate ", " $ tail rows})

                                <td.compact style="width:5%" title="#{colWebsite}">
                                    $maybe p <- getCellContentMaybe colWebsite c
                                        $if not (T.null p)
                                            \ #
                                            <a href="#{p}">#{i18nTranslationOf colWebsite}
                |]


        -- if it begins with a number, apply appropriate header ----------------
        catHeader n category =
            [shamlet|
                $case n
                    $of 0
                        <h1>
                            #{translation}
                    $of 1
                        <h2>
                            #{translation}
                    $of 2
                        <h3>
                            <i>#{translation}
                    $of 3
                        <h4>
                            #{translation}
                    $of 4
                        <h5>
                            #{translation}
                    $of 5
                        <h6>
                            #{translation}
                    $of _
                        <b>
                            #{translation}
            |]
          where
            translation = i18nTranslationOf category

        -- course category div -------------------------------------------------
        {-
            Group the table rows into lists of table rows based on the Category
            they belong to. Then loop over all cateogy based lists to generate
            the different listings.
        -}
        withCat n rows f =
            [shamlet|
                $forall groupedRows <- L.groupBy (catGroup cnf n) rows
                    <div.courses>
                        $maybe category <- catAt cnf n (head groupedRows)
                            #{catHeader n category}
                        #{f groupedRows}
            |]

        -- put everything together ---------------------------------------------
    in [shamlet|
        \<!-- title: #{lookupLang lang $ pageTitle page} -->
        \<!-- fi (Suomenkielinen versio): #{toUrlPath $ lookupLang "fi" $ pageUrl page} -->
        \<!-- se (Svensk version): #{toUrlPath $ lookupLang "se" $ pageUrl page} -->
        \<!-- en (English version): #{toUrlPath $ lookupLang "en" $ pageUrl page} -->
        \

        \<!-- !!! IMPORTANT !!! -->
        \<!-- THIS PAGE IS GENERATED AUTOMATICALLY - DO NOT EDIT DIRECTLY! -->
        \<!-- See https://github.com/SimSaladin/opetussivut for information on how to edit instead -->
        \

        <p>
            $with pg <- head pages
                <a href="#{toUrlPath $ lookupLang lang $ pageUrl pg}">#{lookupLang lang $ pageTitle pg}
            $forall pg <- tail pages
                |
                <a href="#{toUrlPath $ lookupLang lang $ pageUrl pg}">#{lookupLang lang $ pageTitle pg}


        #{markdown def $ LT.fromStrict $ i18nTranslationOf "aputeksti"}


        <div>
            <div.buttons>
                #{i18nTranslationOf "Kieli"}:&nbsp;
                <select id="select-kieli" name="kieli" onchange="updateList(this)">
                    <option value="any">#{i18nTranslationOf "Kaikki"}
                    $forall l <- languages
                        <option value="#{l}">#{i18nTranslationOf l}

                #{i18nTranslationOf "Taso"}:&nbsp;
                <select id="select-taso" name="taso" onchange="updateList(this)">
                    <option value="any" >#{i18nTranslationOf "Kaikki"}
                    $forall cat <- (categories !! categoryLevelTaso)
                        <option value="#{cat}">#{i18nTranslationOf cat}

                #{i18nTranslationOf "Lukukausi"}:&nbsp;
                <select id="select-lukukausi" name="lukukausi" onchange="updateList(this)">
                    <option value="any"   >#{i18nTranslationOf "Kaikki"}
                    <option value="kevät" >#{i18nTranslationOf "Kevät"}
                    <option value="syksy" >#{i18nTranslationOf "Syksy"}
                    <option value="kesä"  >#{i18nTranslationOf "Kesä"}

            <div.headers>
                <table style="width:100%">
                    <tr>
                        <td style="width:10%">#{i18nTranslationOf colCode}
                        <td style="width:57%">#{i18nTranslationOf colCourseName}
                        <td style="width:8%" >#{i18nTranslationOf colPeriod}
                        <td style="width:8%" >#{i18nTranslationOf colRepeats}
                        <td style="width:12%" >#{i18nTranslationOf colLang}
                        <td style="width:5%">#{i18nTranslationOf colWebsite}

            #{courseTable 0 $ tail tableContent}

        <p>
            #{i18nTranslationOf "Päivitetty"} #{show time}

        <style>
            .buttons {
                padding:1em;
            }

            .headers table {
                width:100%;
                table-layout:fixed;
            }

            .courses table {
                width:100%;
                table-layout:fixed;
            }

            .courses td.compact {
                overflow:hidden;
                text-overflow:ellipsis;
                white-space:nowrap;
            }

            tr[data-pidetaan="next-year"] {
                color:gray;
            }

        <script type="text/javascript">
            #{preEscapedToHtml $ renderJavascript $ jsLogic undefined}
    |]


-- | Creating the javascript functions of the buttons in the HTML files.
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


-- ===========================================================================
-- * Get source
-- ===========================================================================


-- | The main entry of the application. This function handles the command line
-- arguments for caching and fetching the wiki tables.
--
-- Fetch a confluence doc (wiki page) by id. This function reads the wiki
-- page with the given page ID, and parses it as an XML-document. The result
-- is cleaned with the 'regexes' function.
getData :: String                   -- ^ Argument: The page ID.
        -> M (Maybe XML.Document)   -- ^ Return:   The cleaned XML-document, if found any.
getData pageId = do
    Config{..} <- ask
    xs         <- lift getArgs
    let file   = cacheDir <> "/" <> pageId <> ".html"

    str <- lift $ case xs of
        "cache" : _ -> Just <$> LT.readFile file
        "fetch" : _ -> do 
                        r <- LT.decodeUtf8 <$> simpleHttp (fetchUrl ++ pageId)
                        if "<title>Log In" `LT.isInfixOf` r
                            then 
                                trace (" * Private Wiki Table - Can't read...")
                                return Nothing
                            else
                                trace (" * Writing to file: " ++ file)
                                LT.writeFile file r >> return (Just r)
        _           -> putStrLn " * Usage: opetussivut <fetch|cache>" >> exitFailure

    return $ cleanAndParse <$> str


-- | This function takes a whole wiki table in 'Text' form and removes some standard
-- HTML tags from the text (see 'regexes' for more information). It then parses an XML
-- document from the HTML bodied 'Text' stream.
--
-- Uses the 'XML.decodeHtmlEntities' setting to decode the 'Text' into XML.
cleanAndParse :: LT.Text        -- ^ Argument: The raw 'Text' version of the cached wiki page.
              -> XML.Document   -- ^ Return:   The XML (HTML) version of the 'Text'.
cleanAndParse = XML.parseText_ parseSettings . LT.pack . foldl1 (.) regexes . LT.unpack
  where
    parseSettings = XML.def { XML.psDecodeEntities = XML.decodeHtmlEntities }


-- ===========================================================================
-- * Parse doc
-- ===========================================================================


-- | 
parseTable :: XML.Document  -- ^ Argument: A 'XML.Document' prepared with the 'cleanAndParse' function.
           -> M Table       -- ^ Return:   The parsed 'Table'.
parseTable doc = head . catMaybes . findTable (fromDocument doc) <$> ask


-- | Looks for all tables in the generated XML document, with XML-attribute /class/ @confluenceTable@,
-- maps the function 'processTable' to the result list, and finally returns a list containing the 
-- discovered 'Table's.
findTable :: Cursor         -- ^ Argument: XML document cursor.
          -> Config         -- ^ Argument: Pointer to the /config.yaml/ data.
          -> [Maybe Table]  -- ^ Return:   List of 'Maybe' 'Table's 
findTable c cnf = map ($| processTable cnf) (c $.// attributeIs "class" "confluenceTable" :: [Cursor])


-- | This function takes a pointer to the 'Config' data and an XML 'Cursor' (pointing to the class
-- attributes with the value @confluenceTable@). It picks out all the XML-elements beginning with
-- the tag @tr@ from the @confluenceTable@ class 'Cursor' and maps all child elements of the @tr@
-- element to it.
--
-- The 'cells' variable contains all the rows of the table. The first row contains some higher-level
-- headers (eg. @Vastaava opettaja@), hence this row is ignored. The second row contains the main 'Header's
-- of the different columns, and the rest of the rows are either 'Course' information or 'Category's
-- separating the different 'Course' informations.
--
-- The first column in the main 'Header' row is empty (this is the column containing the category headers
-- when looking at the Wiki Table).
processTable :: Config      -- ^ Argument: Pointer to the /config.yaml/ file.
             -> Cursor      -- ^ Argument: XML document 'Cursor' pointing at @confluenceTable@ /class/ attribute.
             -> Maybe Table -- ^ Return: The processed 'Maybe' 'Table'.
processTable cnf c = case cells of
    _ : headersRaw : xs ->
        let headers       = tail (mapMaybe getHeader headersRaw)
            (_, mcourses) = L.mapAccumL (getRow cnf headers) [] xs
        in Just $ Table (unsafePerformIO getCurrentTime) headers (catMaybes mcourses)
    _               -> Nothing
  where
    cells = map ($/ anyElement) (c $// element "tr")


-- TODO: Is it possible to remove the 'Maybe' type from this?
-- | Create a 'Maybe' 'Header' type corresponding to the value of the raw XML-cell containging information
-- about the header.
getHeader :: Cursor         -- ^ Argument: Pointer to the cell containing information about the header.
          -> Maybe Header   -- ^ Return:   A 'Maybe' 'Header' type corresponding to the 'Cursor' value.
getHeader c = return . T.toLower . normalize $ T.unwords (c $// content)


-- | A row is either a category or a course. The @['Category']@ is used as an
-- accumulator.
getRow :: Config                        -- ^ Argument: 
       -> [Header]                      -- ^ Argument: 
       -> [Category]                    -- ^ Argument: 
       -> [Cursor]                      -- ^ Argument: 
       -> ([Category], Maybe Course)    -- ^ Return:   
getRow cnf@Config{..} headers cats cs = map (T.unwords . ($// content)) cs `go` head (cs !! 1 $| attribute "class")
  where
    go []        _       = (cats, Nothing)
    go (mc : vs) classes = case toCategory cnf mc of
            Just cat                        -> (accumCategory cnf cat cats, Nothing)
            Nothing | null vs               -> (cats, Nothing)
                    | T.null (normalize mc) -> (cats, Just $ toCourse cnf cats headers (classCur `T.isInfixOf` classes) vs)
                    | otherwise             -> (cats, Just $ toCourse cnf cats headers (classCur `T.isInfixOf` classes) vs)


-- ===========================================================================
-- ** Courses and categories
-- ===========================================================================


-- TODO: Understand what this does
-- | 
toCategory :: Config            -- ^ Argument: 
           -> Text              -- ^ Argument: 
           -> Maybe Category    -- ^ Return:   
toCategory Config{..} t = do
    guard $ t /= "\160" && t /= "syksy" && t /= "kevät"
    guard $ isJust $ L.find (`T.isInfixOf` t) $ concat categories
    return $ normalize t


-- TODO: Understand what this code exactly does.
-- | Accumulate a 'Category' to a list of 'Category's based on what categories
-- cannot overlap.
accumCategory :: Config         -- ^ Argument:
              -> Category       -- ^ Argument: 
              -> [Category]     -- ^ Argument: 
              -> [Category]     -- ^ Return:   
accumCategory Config{..} c cs = case L.findIndex (any (`T.isPrefixOf` c)) categories of
    Nothing -> error $ "Unknown category: " ++ show c
    Just i  -> L.deleteFirstsBy T.isPrefixOf cs (f i) ++ [c]
  where f i = concat $ L.drop i categories


-- TODO: Add an alternative for 'kesä, kenttä'
-- | Creates a row for the current 'Table'. The output will differ depending 
-- on the content in the 'Config' data and the different arguments.
--
-- This function will return the finished row, containing the separating
-- 'Category's and the correct course information from the source 'Table'.
toCourse :: Config          -- ^ Argument: The 'Config' to lookup page configuration data from.
         -> [Category]      -- ^ Argument: A list of 'Category's to pass on to the finished row.
         -> [Header]        -- ^ Argument: A list of 'Header's to select correct 'Text' from the given list.
         -> Bool            -- ^ Argument: If 'True' this will make the course available this year.
         -> [Text]          -- ^ Argument: Used to fill the columns of the row with values from the source 'Table'.
         -> Course          -- ^ Return:   The finished row.
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


-- | Change the format of the @colLang@ column 'Header' in the source 'Table' to be
-- in correct.
--
--          * Replace different ways of writing languages to the correct 'Lang' format.
--
--          * Replace word separators to single spaces.
doLang :: Text      -- ^ Argument: The line of 'Text', that needs conversion.
       -> Text      -- ^ Return:   The correctly formatted 'Text'.
doLang = T.replace "suomi" "fi" . T.replace "eng" "en" . T.replace "englanti" "en"
       . T.replace "ruotsi" "se"
       . T.unwords . T.words
       . T.replace "," " " . T.replace "." " " . T.replace "/" " " . T.toLower


-- | Checks the column @/toistuu/@ from the source 'Table'. If there's anything but
-- numericals or non-alpha signs, it'll return a 'string' consisting of the '-' sign.
-- Else it returns the value of the cell.
doRepeats :: Text   -- ^ Argument: The 'Text' in the cell of the column.
          -> Text   -- ^ Return:   The value of 'Text' argument if there isn't any alpha characters in the cell.
doRepeats x | T.any isLetter x = "-"
            | otherwise        = x


-- | The 'Course' in this case is a row in the 'Table'. This function compares two rows
-- and checks if the values are 'Category's, it will compare the 'Text's of them.
-- It will return true if both of the applied 'Course' arguments have the same text.
catGroup :: Config      -- ^ Argument: Used to access all available @categories@ from /config.yaml/.
         -> Int         -- ^ Argument: Category at level @n@ in /config.yaml/.
         -> Course      -- ^ Argument: First 'Table' row to compare.
         -> Course      -- ^ Argument: Second 'Table' row to compare.
         -> Bool        -- ^ Return:   True if the two rows have the same 'Category' 'Text'.
catGroup cnf n = (==) `on` catAt cnf n


-- | Returns the value of the first found 'Category' that matches the @categories@ at
-- level @n@ in /config.yaml/.
catAt :: Config         -- ^ Argument: Used to access all available @categories@ from /config.yaml/.
      -> Int            -- ^ Argument: Category at level @n@ in /config.yaml/.
      -> Course         -- ^ Argument: A 'Table' row consisting of only a 'Category' 'Text'.
      -> Maybe Text     -- ^ Return:   The first found value matching @categories@ at level @n@.
catAt Config{..} n (cats, _) =
    case [ c | c <- cats, cr <- categories !! n, cr `T.isPrefixOf` c ] of
        x:_ -> Just x
        _   -> Nothing


-- | Get the content of a specific cell from the specified row. This function
-- works as a wrapper for the @'getCellContentMaybe'@ function, stripping it
-- of the 'Maybe' monad.
--
-- If the @'getCellContentMaybe'@ returns a 'Nothing' it'll return a 'Text'
-- saying that the /key couldn't be found/, otherwise it returns the value
-- of the cell.
getCellContent :: Text      -- ^ Argument: The 'Text' to look for in the row consisting of the 'Course' type.
               -> Course    -- ^ Argument: The row to look in.
               -> Text      -- ^ Return:   The result 'Text'.
getCellContent k c = fromMaybe (traceShow ("Key not found" :: String, k, c) $ "Key not found: " <> k) $ getCellContentMaybe k c


-- | Get the content of a specific cell form the specified row.
--
-- Returns 'Nothing' if the key isn't in the row, otherwise it returns
-- the value of the cell.
getCellContentMaybe :: Text           -- ^ Argument: The 'Text' to look for in the row.
                    -> Course         -- ^ Argument: The row to look in.
                    -> Maybe Text     -- ^ Return:   The 'Text' of the cell if it was found otherwise 'Nothing'.
getCellContentMaybe k (_, c) = Map.lookup k c



