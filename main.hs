{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Main
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- 
--     /opetus/kurssit.body
--     /svenska/studierna/kurser.body
--     /english/studying/courses.body
--
--     /opetus/kurssit/{aineopinnot,perusopinnot,muutopinnot,syventavatopinnot}.body
--     /svenska/studierna/{...}.body
--     /english/studying/{...}.body
--
------------------------------------------------------------------------------
module Main where

import Prelude
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Reader
import           Data.Function              (on)
import qualified Data.List          as L
import           Data.Map                   (Map)
import qualified Data.Map           as Map
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Text.Lazy     as LT
import qualified Data.Text.Lazy.IO  as LT
import qualified Data.Yaml          as Yaml
import           Network.HTTP.Conduit       (simpleHttp)
import           Text.Printf
import           Text.Blaze.Html            (preEscapedToHtml)
import           Text.Blaze.Renderer.Text   (renderMarkup)
import           Text.Hamlet
import           Text.Julius
import           Text.Regex
import qualified Text.XML           as XML
import           Text.XML.Cursor
import           Debug.Trace
import           Data.Time
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Generics

-- * Configuration

type Lang = Text -- En, Se, Fi, ...

data Config = Config
            { pageId, pageUrl :: String
            , colCode, colLang, colCourseName, colRepeats, colPeriod, colWebsite :: Text
            , colLangFi, colLukukausi, classCur :: Text
            , categories :: [[Text]]
            , i18n :: I18N
            , languages :: [Lang]
            } deriving Generic

instance Yaml.FromJSON Config

-- | A hack, for confluence html is far from the (strictly) spec.
regexes :: [String -> String]
regexes = [ rm "<link [^>]*>", rm "<link [^>]*\">", rm "<img [^>]*>" ]
    where rm s i = subRegex (mkRegexWithOpts s False True) i ""

pages      = [ ("Kaikki kurssit", "/opetus/kurssit", id) ]
toPath     = ("testi" <>)
toUrlPath  = ("/opetus/" <>) . toPath . (<> ".html")
toFilePath = T.unpack . toPath . (<> ".body")

-- * Main

type M = ReaderT Config IO

main :: IO ()
main = Yaml.decodeFileEither "config.yaml" >>= either (error . show) (runReaderT go)

go :: M ()
go = do
    table <- getData >>= parseTable
    Config{..} <- ask
    forM_ pages $ \(title, url, f) -> forM_ languages $ \lang ->
        renderTable lang title url (f table) (toFilePath $ toLang i18n lang url)

-- * Types

-- | Source table
data Table = Table UTCTime [Header] [Course] deriving (Show, Read)

type I18N = Map Text (Map Lang Text)

-- | td in source table
type ContentBlock = Text

-- | Column headers in source table
type Header = Text

-- | First column in source table
type Category = Text

-- | Row is source table
type Course = ([Category], Map Header ContentBlock)

-- * HTML

renderTable :: Lang -> Text -> Text -> Table -> FilePath -> M ()
renderTable lang title url table fp = ask >>= lift . LT.writeFile fp . renderMarkup . tableBody lang title url table

-- | How to render the data
tableBody :: Lang -> Text -> Text -> Table -> Config -> Html
tableBody lang title url (Table time _ stuff) cnf@Config{..} =
        let ii      = toLang i18n lang
            getLang = getThingLang i18n
            in [shamlet|
\<!-- title: #{ii title} -->
\<!-- fi (Suomenkielinen versio): #{toUrlPath $ toLang i18n "fi" url} -->
\<!-- se (Svensk version): #{toUrlPath $ toLang i18n "se" url} -->
\<!-- en (English version): #{toUrlPath $ toLang i18n "en" url} -->
\ 
<p>
  #{ii "Kieli"}:&nbsp;
  <select id="select-kieli" name="kieli" onchange="updateList(this)">
     <option value="any">#{ii "Kaikki"}
     <option value="fi" >#{ii "Suomeksi"}
     <option value="en" >#{ii "Englanniksi"}
     <option value="se" >#{ii "Ruotsiksi"}

  #{ii "Taso"}:&nbsp;
  <select id="select-taso" name="taso" onchange="updateList(this)">
     <option value="any"               >#{ii "Kaikki"}
     <option value="Perusopinnot"      >#{ii "Perusopinnot"}
     <option value="Aineopinnot"       >#{ii "Aineopinnot"}
     <option value="Muut opinnot"      >#{ii "Muut opinnot"}
     <option value="Syventävät opinnot">#{ii "Syventävät opinnot"}

  #{ii "Lukukausi"}:&nbsp;
  <select id="select-lukukausi" name="lukukausi" onchange="updateList(this)">
     <option value="any"   >#{ii "Kaikki"}
     <option value="kevät" >#{ii "Kevät"}
     <option value="syksy" >#{ii "Syksy"}
     <option value="kesä"  >#{ii "Kesä"}
<p>
  #{ii "aputeksti"}

<table style="width:100%">
    <tr>
        <td style="width:10%">#{ii "Koodi"}
        <td style="width:55%">#{ii "Kurssin nimi"}
        <td style="width:7%" >#{ii "Periodi"}
        <td style="width:7%" >#{ii "Toistuu"}
        <td style="width:20%">#{ii "Opetuskieli"}
$forall main <- L.groupBy (mainCategory cnf) stuff
  <div.courses>
    <h1>#{ii $ mainCat cnf $ head main}
    $forall subs <- L.groupBy (subCategory cnf) main
      <div.courses>
        <h2>#{ii $ subCat cnf $ head subs}
        <table style="width:100%">
          $forall c <- subs
            <tr data-taso="#{mainCat cnf c}" data-kieli="#{getThing colLang c}" data-lukukausi="#{getThing colLukukausi c}" data-pidetaan="#{getThing "pidetään" c}">
              <td style="width:10%">
                <a href="https://weboodi.helsinki.fi/hy/opintjakstied.jsp?html=1&Kieli=1&Tunniste=#{getThing colCode c}">
                  <b>#{getThing colCode c}

              <td style="width:55%">#{getLang lang colCourseName c} #
                $with op <- getThing "op" c
                    $if not (T.null op)
                         (#{op} #{ii "op"})

              <td.compact style="width:7%"  title="#{getThing colPeriod c}">#{getThing colPeriod c}
              <td.compact style="width:7%"  title="#{getThing colRepeats c}">#{getThing colRepeats c}
              <td.compact style="width:20%" title="#{getThing colLangFi c}">#{getThing colLangFi c}
                $maybe p <- getThingMaybe colWebsite c
                  <a href="#{p}">#{ii "Kotisivu"}

<p>
    #{ii "Päivitetty"} #{show time}
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

toLang :: I18N -> Lang -> Text -> Text
toLang db lang key = case Map.lookup key db of
    Just db' -> case Map.lookup lang db' of
        Just val -> val
        Nothing
            | lang == "fi" -> key
            | otherwise    -> trace ("Warn: no i18n for key `" ++ T.unpack key ++ "' with lang `" ++ show lang ++ "'") key
    Nothing -> trace ("Warn: no i18n db for key `" ++ T.unpack key ++ "'") key

getThingLang :: I18N -> Lang -> Text -> Course -> Text
getThingLang db lang key c = fromMaybe (getThing key c) $ getThingMaybe (toLang db lang key) c

-- * JS

--
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

-- * Courses and categories

toCourse :: Config -> [Category] -> [Header] -> Bool -> [Text] -> Course
toCourse Config{..} cats hs iscur xs =
    (cats, Map.adjust toLang colLang $
           Map.insert "pidetään" (if iscur then "this-year" else "next-year") $
           Map.insert colLangFi fiLangs $
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

        toLang x | x == "suomi" = "fi"
                 | "suomi" `T.isInfixOf` x, "eng" `T.isInfixOf` x = "fi, en"
                 | "eng" `T.isInfixOf` x = "en"
                 | otherwise = "fi, en, se"

        fiLangs = case Map.lookup colLang vals of
                      Just x  -> x -- T.replace "fi" "suomi" $ T.replace "en" "englanti" $ T.replace "se" "ruotsi" x
                      Nothing -> "?"

normalize =
    T.dropAround (`elem` " ,-!")
    . T.replace "ILMOITTAUTUMINEN PUUTTUU" ""
    . T.unwords . map (T.unwords . T.words) . T.lines

-- | Accumulate a category to list of categories based on what categories
-- cannot overlap
accumCategory :: Config -> Category -> [Category] -> [Category]
accumCategory Config{..} c cs = L.nub $ c : maybe (c : cs) (L.deleteFirstsBy T.isInfixOf cs) ci
    where ci = L.find (isJust . L.find (`T.isPrefixOf` c)) categories

toCategory :: Config -> Text -> Maybe Category
toCategory Config{..} t = do
    guard $ t /= "\160" && t /= "syksy" && t /= "kevät"
    guard $ isJust $ L.find (`T.isInfixOf` t) $ concat categories
    return $ normalize t

getThing :: Text -> Course -> Text
getThing k c = fromMaybe (traceShow ("Key not found", k, c) $ "Key not found: " <> k) $ getThingMaybe k c

-- | Level one category
mainCat :: Config -> Course -> Text
mainCat Config{..} (cats, _)
    | Just c <- L.find (`elem` (categories !! 0)) cats = c
    | otherwise = "(Tuntematon)"

subCat :: Config -> Course -> Text
subCat Config{..} (cats, _)
    | Just c <- L.find (maybe False (`elem` ['0'..'9']) . fmap fst . T.uncons) cats = c
    | otherwise = "Fysiikka"
    -- ^ TODO this thing makes no sense whatsoever

getThingMaybe :: Text -> Course -> Maybe Text
getThingMaybe k (_, c) = Map.lookup k c

mainCategory, subCategory :: Config -> Course -> Course -> Bool
mainCategory cnf = (==) `on` mainCat cnf
subCategory cnf = (==) `on` subCat cnf

-- * Get source

getData :: M XML.Document
getData = do
    Config{..} <- ask
    let parseSettings = XML.def { XML.psDecodeEntities = XML.decodeHtmlEntities }
    -- XML.parseLBS_ parseSettings <$> simpleHttp (pageUrl ++ pageId)
    -- url
    lift $ XML.parseText_ parseSettings . LT.pack . foldl1 (.) regexes <$> readFile "raw.html"
    -- file

-- ** Parse fetched

parseTable :: XML.Document -> M Table
parseTable doc = head . catMaybes . findTable (fromDocument doc) <$> ask

findTable :: Cursor -> Config -> [Maybe Table]
findTable c cnf = map ($| processTable cnf) (c $.// attributeIs "class" "confluenceTable" :: [Cursor])

getHeader :: Cursor -> Maybe Header
getHeader = go . normalize . T.unwords . ($// content)
    where go "" = Nothing
          go x = Just x

processTable :: Config -> Cursor -> Maybe Table
processTable cnf c = table
  where
    cells :: [[Cursor]]
    cells = map ($/ element "td") (c $// element "tr")

    table = case cells of
        (_ : header : xs) ->
            let headers   = mapMaybe getHeader header
                (ac, mcs) = L.mapAccumL (getRow cnf headers) [] xs
                courses   = catMaybes mcs
            in Just $ Table (unsafePerformIO getCurrentTime) headers courses
        _ -> Nothing

-- | A row is either a category or course
getRow :: Config -> [Header] -> [Category] -> [Cursor] -> ([Category], Maybe Course)
getRow cnf@Config{..} hs cats cs = map ($// content) cs `go` ((cs !! 1 $| attribute "class") !! 0)
    where go :: [[Text]] -> Text -> ([Category], Maybe Course)
          go (mc : vs) classes = case toCategory cnf (head mc) of
                Just cat -> (accumCategory cnf cat cats, Nothing)
                Nothing | null vs                      -> (cats, Nothing)
                        | T.null (normalize (head mc)) -> (cats, Just $ toCourse cnf cats hs (classCur `T.isInfixOf` classes) $ map T.unwords vs)
                        | otherwise                    -> (cats, Just $ toCourse cnf cats hs (classCur `T.isInfixOf` classes) $ map T.unwords vs)

-- * Debugging

ppCourse :: Course -> IO ()
ppCourse (cats, vals) = do
    T.putStrLn " ---------------------------------------- "
    printf "%-28s: %s\n" ("Categories" :: String) (T.unpack $ T.intercalate ", " cats)
    mapM_ (\(h, v) -> printf "%-28s: %s\n" (T.unpack h) (T.unpack v)) $ Map.toList vals
