{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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

-- * Configuration

-- | Page to fetch
pageId = "133436923"

-- | Where to get the source table
url :: String
url = "https://wiki.helsinki.fi/plugins/viewsource/viewpagesrc.action?pageId=" ++ pageId

-- | Columns in source table
colLang       = "opetuskieli"
colCode       = "Koodi"
colLangFi     = "kieli-fi"
colCourseName = "Kurssin nimi"
colWebsite    = "Kotisivu"
colPeriod     = "Periodi"
colLukukausi  = "Lukukausi" -- kevät, kesä, syksy

classCur = "highlight-green"

-- List of categories
categories :: [[Text]]
categories =
    [ [ "Perusopinnot", "Aineopinnot", "Syventävät opinnot", "Muut opinnot" ]
    , [ "Pääaineopetus" ]
    , [ "Pakolliset opinnot", "Valinnaiset opinnot" ]
    , [ "1. ", "2. ", "3. ", "4.", "5. ", "6. ", "7. ", "8. ", "9. " ]
    , [ "A. ", "B. " ]
    , [ "kevät", "syksy" ]
    ]

data Lang = Fi | En | Se
          deriving (Show, Read, Eq, Ord)

i18n :: Map Text (Map Lang Text)
i18n = Map.fromList
    [ ("Perusopinnot",       Map.fromList [ (En, "Basic studies"), (Se, "Grundstudier") ])
    , ("Aineopinnot",        Map.fromList [ (En, "Intermediate studies"), (Se, "Ämnesstudier") ])
    , ("Syventävät opinnot", Map.fromList [ (En, "Advanced studies"), (Se, "Advancerade studier") ])
    , ("Muut opinnot",       Map.fromList [ (En, "Other studies"), (Se, "Andra studier") ])

    , ("Kaikki",       Map.fromList [ (En, "All"), (Se, "Allt") ])
    , ("Kieli",        Map.fromList [ (En, "Language"), (Se, "Språk") ])
    , ("Kevät",        Map.fromList [ (En, "Spring"), (Se, "Vår") ])
    , ("Syksy",        Map.fromList [ (En, "Autumn"), (Se, "Höst") ])
    , ("Kesä",         Map.fromList [ (En, "Summer"), (Se, "Sommar") ])
    , ("Kurssin nimi", Map.fromList [ (En, "Course name"), (Se, "Kursnamn") ])
    , ("Lukukausi",    Map.fromList [ (En, "Semester"), (Se, "Termin") ])
    , ("Taso",         Map.fromList [ (En, "Level"), (Se, "Level") ])

    , ("Englanniksi", Map.fromList [ (En, "In English"), (Se, "På engelska") ])
    , ("Suomeksi",    Map.fromList [ (En, "In Finnish"), (Se, "På finska") ])
    , ("Ruotsiksi",   Map.fromList [ (En, "In Swedish"), (Se, "På svenska") ])

    -- Titles
    , ("Kaikki kurssit", Map.fromList [ (En, "All courses"), (Se, "Alla kurser") ])

    -- urls
    , ("/opetus/kurssit", Map.fromList [ (En, "/english/studying/courses"), (Se, "/svenska/studierna/kurser") ])
    ]

-- | A hack, for confluence html is far from the (strictly) spec.
regexes :: [String -> String]
regexes =
    [ rm "<link [^>]*>", rm "<link [^>]*\">", rm "<img [^>]*>"
    -- , rm "<meta [^>]*>", rm "<meta [^>]*\">"
    -- , rm "<br[^>]*>", rm "<input [^>]*>"
    -- , rm "</?ol[^>]*>", rm "</?li[^>]*>", rm "</?ul[^>]*>"
    -- , rm "<div id=\"footer\".*section>[^<]*</div>"
    ] where rm s i = subRegex (mkRegexWithOpts s False True) i ""

pages      = [ ("Kaikki kurssit", "/opetus/kurssit", id) ]
languages  = [Fi, En, Se]
toPath     = ("testi" <>)
toUrlPath  = ("/opetus/" <>) . toPath . (<> ".html")
toFilePath = T.unpack . toPath . (<> ".body")

-- * Main

main :: IO ()
main = do
    doc <- getDataFile "raw.html"
    let table = parseTable doc
    forM_ pages $ \(title, url, f) ->
        forM_ languages $ \lang ->
            renderTable lang title url (f table) (toFilePath $ toLang i18n lang url)

-- * Types

-- | Source table
data Table = Table [Header] [Course] deriving (Show, Read)

-- | td in source table
type ContentBlock = Text

-- | Column headers in source table
type Header = Text

-- | First column in source table
type Category = Text

-- | Row is source table
type Course = ([Category], Map Header ContentBlock)

-- * HTML

renderTable :: Lang -> Text -> Text -> Table -> FilePath -> IO ()
renderTable lang title url table fp =
    LT.writeFile fp $ renderMarkup $ tableBody lang title url table

-- | How to render the data
tableBody :: Lang -> Text -> Text -> Table -> Html
tableBody lang title url (Table _ stuff) =
        let ii = toLang i18n lang
            in [shamlet|
\<!-- title: #{ii title} -->
\<!-- fi (Suomenkielinen versio): #{toUrlPath $ toLang i18n Fi url} -->
\<!-- se (Svensk version): #{toUrlPath $ toLang i18n Se url} -->
\<!-- en (English version): #{toUrlPath $ toLang i18n En url} -->
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
  #{ii "Klikkaa kurssikoodista Weboodiin"}.

$forall main <- L.groupBy mainCategory stuff
  <div.courses>
    <h1>#{ii $ mainCat $ head main}
    $forall subs <- L.groupBy subCategory main
      <div.courses>
        <h2>#{ii $ subCat $ head subs}
        <table style="width:100%">
          $forall c <- subs
            <tr data-taso="#{mainCat c}" data-kieli="#{getThing colLang c}" data-lukukausi="#{getThing colLukukausi c}" data-pidetaan="#{getThing "pidetään" c}">
              <td style="width:10%">
                <a href="https://weboodi.helsinki.fi/hy/opintjakstied.jsp?html=1&Kieli=1&Tunniste=#{getThing colCode c}">
                  <b>#{getThing colCode c}

              <td style="width:62%">
                #{getThingLang lang colCourseName c}

              <td style="width:7%">
                #{getThing colPeriod c}

              <td style="width:20%">
                #{getThing colLangFi c}
                $maybe p <- getThingMaybe colWebsite c
                  <a href="#{p}">#{ii "Kotisivu"}
<style>
    tr[data-pidetaan="next-year"] {
        color:gray;
    }

<script type="text/javascript">
  #{preEscapedToHtml $ renderJavascript $ jsLogic undefined}
|]

toLang :: Map Text (Map Lang Text) -> Lang -> Text -> Text
toLang db lang key = case Map.lookup key db of
    Just db' -> case Map.lookup lang db' of
        Just val -> val
        Nothing
            | lang == Fi -> key
            | otherwise  -> trace ("Warn: no i18n for key `" ++ T.unpack key ++ "' with lang `" ++ show lang ++ "'") key
    Nothing -> trace ("Warn: no i18n db for key `" ++ T.unpack key ++ "'") key

getThingLang :: Lang -> Text -> Course -> Text
getThingLang lang key c = fromMaybe (getThing key c) $ getThingMaybe (toLang i18n lang key) c

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

toCourse :: [Category] -> [Header] -> Bool -> [Text] -> Course
toCourse cats hs iscur xs =
    (cats, Map.adjust toLang colLang    $
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
accumCategory :: Category -> [Category] -> [Category]
accumCategory c cs = L.nub $ c : maybe (c : cs) (L.deleteFirstsBy T.isInfixOf cs) ci
    where ci = L.find (isJust . L.find (`T.isPrefixOf` c)) categories

toCategory :: Text -> Maybe Category
toCategory t = do guard $ t /= "\160" && t /= "syksy" && t /= "kevät"
                  guard $ isJust $ L.find (`T.isInfixOf` t) $ concat categories
                  return $ normalize t

getThing :: Text -> Course -> Text
getThing k c = fromMaybe (traceShow ("Key not found", k, c) $ "Key not found: " <> k) $ getThingMaybe k c

-- | Level one category
mainCat :: Course -> Text
mainCat (cats, _)
    | Just c <- L.find (`elem` (categories !! 0)) cats = c
    | otherwise = "(Tuntematon)"

subCat :: Course -> Text
subCat (cats, _)
    | Just c <- L.find (maybe False (`elem` ['0'..'9']) . fmap fst . T.uncons) cats = c
    | otherwise = "Fysiikka"
    -- ^ TODO this thing makes no sense whatsoever

getThingMaybe :: Text -> Course -> Maybe Text
getThingMaybe k (_, c) = Map.lookup k c

mainCategory, subCategory :: Course -> Course -> Bool
mainCategory = (==) `on` mainCat
subCategory = (==) `on` subCat

-- * Get source

getData :: String -> IO XML.Document
getData = liftM (XML.parseLBS_ parseSettings) . simpleHttp
parseSettings = XML.def { XML.psDecodeEntities = XML.decodeHtmlEntities }

getDataFile :: FilePath -> IO XML.Document
getDataFile fp = do
    txt <- readFile fp
    return $ XML.parseText_ parseSettings $ LT.pack $ foldl1 (.) regexes txt

-- ** Parse fetched

parseTable :: XML.Document -> Table
parseTable = head . catMaybes . findTable . fromDocument

findTable :: Cursor -> [Maybe Table]
findTable c = map ($| processTable) (c $.// attributeIs "class" "confluenceTable" :: [Cursor])

getHeader :: Cursor -> Maybe Header
getHeader = go . normalize . T.unwords . ($// content)
    where go "" = Nothing
          go x = Just x

processTable :: Cursor -> Maybe Table
processTable c = table
  where
    cells :: [[Cursor]]
    cells = map ($/ element "td") (c $// element "tr")

    table = case cells of
        (_ : header : xs) ->
            let headers   = mapMaybe getHeader header
                (ac, mcs) = L.mapAccumL (getRow headers) [] xs
                courses   = catMaybes mcs
            in Just $ Table headers courses
        _ -> Nothing

-- | A row is either a category or course
getRow :: [Header] -> [Category] -> [Cursor] -> ([Category], Maybe Course)
getRow hs cats cs = go (map ($// content) cs)
                       ((cs !! 1 $| attribute "class") !! 0)
    where go :: [[Text]] -> Text -> ([Category], Maybe Course)
          go (mc : vs) classes = case toCategory (head mc) of
                Just cat -> (accumCategory cat cats, Nothing)
                Nothing | null vs                      -> (cats, Nothing)
                        | T.null (normalize (head mc)) -> (cats, Just $ toCourse cats hs (classCur `T.isInfixOf` classes) $ map T.unwords vs)
                        | otherwise                    -> (cats, Just $ toCourse cats hs (classCur `T.isInfixOf` classes) $ map T.unwords vs)

-- * Debugging

ppCourse :: Course -> IO ()
ppCourse (cats, vals) = do
    T.putStrLn " ---------------------------------------- "
    printf "%-28s: %s\n" ("Categories" :: String) (T.unpack $ T.intercalate ", " cats)
    mapM_ (\(h, v) -> printf "%-28s: %s\n" (T.unpack h) (T.unpack v)) $ Map.toList vals
