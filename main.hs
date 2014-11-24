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
------------------------------------------------------------------------------
module Main where

import Prelude hiding (readFile)
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
import qualified Data.Text.Lazy.IO as LT
import qualified Data.List as L
import Text.XML
import Text.XML.Cursor
import Debug.Trace
import Text.Hamlet
import Text.Blaze.Renderer.Text (renderMarkup)

-- * Configure

url = "https://wiki.helsinki.fi/display/fysoppiaine/HY+fysiikan+oppiaineen+kurssisuunnitelma+s2014-k2016"

categories :: [[Text]]
categories =
    [ [ "Pääaineopetus" ]
    , [ "Perusopinnot", "Aineopinnot", "Syventävät opinnot" ]
    , [ "Pakolliset opinnot", "Valinnaiset opinnot", "Muut opinnot" ]
    , [ "1. ", "2. ", "3. ", "4.", "5. ", "6. ", "7. ", "8. ", "9. " ]
    , [ "A. ", "B. " ]
    , [ "kevät", "syksy" ]
    ]

mainCat :: Course -> Text
mainCat (cats, _)
    | Just c <- L.find (\x -> T.head x `elem` ['0'..'9']) cats = c
    | otherwise = "Fysiikka"
    -- ^ TODO this thing makes no sense whatsoever

-- * main

main = do
    -- doc <- getData url
    doc <- readFile parseSettings "content.html"
    putStrLn "Read file"
    processDoc doc

-- * Types

-- | First column
type Category = Text
type Course = ([Category], Map Header ContentBlock)

-- | td
type ContentBlock = Text

-- | Column headers
type Header = Text

-- | 
data Table = Table [Header] [Course]
           deriving (Show, Read)

-- * Parse and build

processTable :: Cursor -> Maybe Table
processTable c = trace "process" table
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

-- | Accumulate a category to list of categories based on what categories
-- cannot overlap
accumCategory :: Category -> [Category] -> [Category]
accumCategory c cs = L.nub $ c : maybe (c : cs) (L.deleteFirstsBy T.isInfixOf cs) ci
    where ci = L.find (isJust . L.find (`T.isPrefixOf` c)) categories

-- | A row is either a category or course
getRow :: [Header] -> [Category] -> [Cursor] -> ([Category], Maybe Course)
getRow hs cats = go . map ($// content) -- ($// content)
    where go :: [[Text]] -> ([Category], Maybe Course)
          go (mc : vs) = case toCategory (head mc) of
                Just cat -> (accumCategory cat cats, Nothing)
                Nothing  -> (cats, Just $ toCourse cats hs $ map T.unwords vs)

toCategory :: Text -> Maybe Category
toCategory t = do guard $ t /= "\160" && t /= "syksy" && t /= "kevät"
                  return $ normalize t

toCourse :: [Category] -> [Header] -> [Text] -> Course
toCourse cats hs xs = (cats, Map.fromList $ zip hs $ map normalize xs)

normalize = T.unwords . map (T.unwords . T.words) . T.lines

getData :: String -> IO Document
getData = liftM (parseLBS_ parseSettings) . simpleHttp

parseSettings = def { psDecodeEntities = decodeHtmlEntities }

processDoc :: Document -> IO ()
processDoc = renderTable . head . catMaybes . findTable . fromDocument

findTable :: Cursor -> [Maybe Table]
findTable = element "table" &| processTable

getHeader :: Cursor -> Maybe Header
getHeader = go . normalize . T.unwords . ($// content)
    where go "" = Nothing
          go x = Just x

-- * Output

renderTable :: Table -> IO ()
renderTable t@(Table hs cs) = do
    LT.putStrLn $ renderMarkup $ listed t
    -- mapM_ ppCourse cs

ppCourse :: Course -> IO ()
ppCourse (cats, vals) = do
    T.putStrLn " ---------------------------------------- "
    printf "%-28s: %s\n" ("Categories" :: String) (T.unpack $ T.intercalate ", " cats)
    mapM_ (\(h, v) -> printf "%-28s: %s\n" (T.unpack h) (T.unpack v)) $ Map.toList vals

listed :: Table -> Html
listed (Table _ stuff) = [shamlet|
$forall courses <- L.groupBy mainCategory stuff
    <h1>#{mainCat $ head courses}
    <table>
        $forall c <- courses
            <tr>
                <td style="width:67%">#{getThing "Koodi" c} #{getThing "Kurssin nimi" c}
                <td style="width:15%">
                    $maybe p <- getThingMaybe "Kotisivu" c
                        <a href="#{p}">Kotisivu
                <td style="width:13%">
                    <a href="https://weboodi.helsinki.fi/hy/opintjakstied.jsp?html=1&Kieli=1&Tunniste=#{getThing "Koodi" c}">Kuvaus
|]

getThing :: Text -> Course -> Text
getThing k = fromMaybe (error $ "Key not found: " ++ T.unpack k) . getThingMaybe k

getThingMaybe :: Text -> Course -> Maybe Text
getThingMaybe k (_, c) = Map.lookup k c

mainCategory :: Course -> Course -> Bool
mainCategory = (==) `on` mainCat
