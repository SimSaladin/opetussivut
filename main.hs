{-# LANGUAGE OverloadedStrings #-}
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
--     /opetus/index.html
--     /svenska/studierna/index.body
--     /english/studying/index.body
--
--     /opetus/kurssit.html
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Function (on)
import Control.Monad
import Text.Printf
import Data.Maybe
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.List as L
import qualified Text.XML as XML
import Text.XML.Cursor
import Debug.Trace
import Text.Hamlet
import Text.Julius
import Text.Regex
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Renderer.Text (renderMarkup)

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

-- | A hack, for confluence html is far from the (strictly) spec.
regexes :: [String -> String]
regexes =
    [ rm "<link [^>]*>", rm "<link [^>]*\">", rm "<img [^>]*>"
    -- , rm "<meta [^>]*>", rm "<meta [^>]*\">"
    -- , rm "<br[^>]*>", rm "<input [^>]*>"
    -- , rm "</?ol[^>]*>", rm "</?li[^>]*>", rm "</?ul[^>]*>"
    -- , rm "<div id=\"footer\".*section>[^<]*</div>"
    ] where rm s i = subRegex (mkRegexWithOpts s False True) i ""

-- * Main

main :: IO ()
main = do
    txt <- readFile "raw.html"
    let doc = XML.parseText_ parseSettings $ LT.pack $ foldl1 (.) regexes txt
    processDoc doc

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

renderTable :: Table -> IO ()
renderTable t@(Table hs cs) = LT.putStrLn $ renderMarkup $ tableBody t

-- | How to render the data
tableBody :: Table -> Html
tableBody (Table _ stuff) = [shamlet|
\<!-- title: Kaikki kurssit -->
\<!-- fi (Suomenkielinen versio): /opetus/testi.html -->
\<!-- se (Institutionens hemsida): /svenska/studierna/index.html -->
\<!-- en (English version): /english/studying/index.html -->
\ 
<p>
  Kieli:&nbsp;
  <select id="select-kieli" name="kieli" onchange="updateList(this)">
     <option value="any">Kaikki
     <option value="fi">Suomeksi
     <option value="en">Englanniksi
     <option value="se">Ruotsiksi

  Taso:&nbsp;
  <select id="select-taso" name="taso" onchange="updateList(this)">
     <option value="any">Kaikki
     <option value="Perusopinnot">Perusopinnot
     <option value="Aineopinnot">Aineopinnot
     <option value="Muut opinnot">Muut opinnot
     <option value="Syventävät opinnot">Syventävät

  Lukukausi:&nbsp;
  <select id="select-lukukausi" name="lukukausi" onchange="updateList(this)">
     <option value="any">Kaikki
     <option value="kevät">Kevät
     <option value="syksy">Syksy
     <option value="kesä">Kesä
<p>
  Klikkaa kurssikoodista Weboodiin.

$forall main <- L.groupBy mainCategory stuff
  <div.courses>
    <h1>#{mainCat $ head main}
    $forall subs <- L.groupBy subCategory main
      <div.courses>
        <h2>#{subCat $ head subs}
        <table style="width:100%">
          $forall c <- subs
            <tr data-taso="#{mainCat c}" data-kieli="#{getThing colLang c}" data-lukukausi="#{getThing colLukukausi c}">
              <td style="width:10%">
                <a href="https://weboodi.helsinki.fi/hy/opintjakstied.jsp?html=1&Kieli=1&Tunniste=#{getThing colCode c}">
                  <b>#{getThing colCode c} 

              <td style="width:62%">
                #{getThing colCourseName c}

              <td style="width:7%">
                #{getThing colPeriod c}

              <td style="width:20%">
                #{getThing colLangFi c}
                $maybe p <- getThingMaybe colWebsite c
                  <a href="#{p}">Kotisivu

<script type="text/javascript">
  #{preEscapedToHtml $ renderJavascript $ jsLogic undefined}
|]

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

toCourse :: [Category] -> [Header] -> [Text] -> Course
toCourse cats hs xs = (cats, Map.adjust toLang colLang $ Map.insert colLangFi fiLangs $ Map.insert colLukukausi lukukausi vals)
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
getThing k c = fromMaybe (traceShow (k, c) $ "Key not found: " <> k) $ getThingMaybe k c

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

-- ** Process fetched

processDoc :: XML.Document -> IO ()
processDoc = renderTable . head . catMaybes . findTable . fromDocument

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
getRow hs cats = go . map ($// content) -- ($// content)
    where go :: [[Text]] -> ([Category], Maybe Course)
          go (mc : vs) = case toCategory (head mc) of
                Just cat -> (accumCategory cat cats, Nothing)
                Nothing | null vs                      -> (cats, Nothing)
                        | T.null (normalize (head mc)) -> (cats, Just $ toCourse cats hs $ map T.unwords vs)
                        | otherwise                    -> (cats, Just $ toCourse cats hs $ map T.unwords vs)

-- * Debugging

ppCourse :: Course -> IO ()
ppCourse (cats, vals) = do
    T.putStrLn " ---------------------------------------- "
    printf "%-28s: %s\n" ("Categories" :: String) (T.unpack $ T.intercalate ", " cats)
    mapM_ (\(h, v) -> printf "%-28s: %s\n" (T.unpack h) (T.unpack v)) $ Map.toList vals
