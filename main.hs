{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad
import Text.Printf
import Data.Maybe
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import Text.XML
import Text.XML.Cursor
import Debug.Trace

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
    where go :: [ [Text] ] -> ([Category], Maybe Course)
          go (mc : vs) = case toCategory (head mc) of
                Just cat -> (accumCategory cat cats, Nothing)
                Nothing  -> (cats, Just $ toCourse cats hs $ map head vs)

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

renderTable :: Table -> IO ()
renderTable (Table hs cs) = do
    -- mapM_ T.putStrLn hs
    mapM_ ppCourse cs

findTable :: Cursor -> [Maybe Table]
findTable = element "table" &| processTable

getHeader :: Cursor -> Maybe Header
getHeader = go . normalize . head . ($// content)
    where go "" = Nothing
          go x = Just x

-- * Output

ppCourse :: Course -> IO ()
ppCourse (cats, vals) = do
    T.putStrLn " ---------------------------------------- "
    printf "%-28s: %s\n" ("Categories" :: String) (T.unpack $ T.intercalate ", " cats)
    mapM_ (\(h, v) -> printf "%-28s: %s\n" (T.unpack h) (T.unpack v)) $ Map.toList vals
