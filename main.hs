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

url = "https://wiki.helsinki.fi/display/fysoppiaine/HY+fysiikan+oppiaineen+kurssisuunnitelma+s2014-k2016"

main = do
    -- doc <- getData url
    doc <- readFile parseSettings "content.html"
    putStrLn "Read file"
    processDoc doc

-- | First column
type Category = Text

-- | Accumulate a category to list of categories based on what categories
-- cannot overlap
accumCategory :: Category -> [Category] -> [Category]
accumCategory c cs = c : cs -- TODO implement

type Course = ([Category], Map Header ContentBlock)

-- | td
type ContentBlock = Text

-- | Column headers
type Header = Text

-- | 
data Table = Table [Header] [Course]
           deriving (Show, Read)

getData :: String -> IO Document
getData = liftM (parseLBS_ parseSettings) . simpleHttp

parseSettings = def { psDecodeEntities = decodeHtmlEntities }

processDoc :: Document -> IO ()
processDoc = renderTable . head . catMaybes . findTable . fromDocument

renderTable :: Table -> IO ()
renderTable (Table hs cs) = do
    -- mapM_ T.putStrLn hs
    mapM_ ppCourse cs

ppCourse :: Course -> IO ()
ppCourse (cats, vals) = do
    T.putStrLn " ---------------------------------------- "
    T.putStrLn $ T.unwords cats
    mapM_ (\(h, v) -> printf "%-28s: %s\n" (T.unpack h) (T.unpack v)) $ Map.toList vals

findTable :: Cursor -> [Maybe Table]
findTable = element "table" &| processTable

processTable :: Cursor -> Maybe Table
processTable c = trace "process" table
  where
    cells :: [[Cursor]]
    cells = map ($/ element "td") (c $// element "tr")

    table = case cells of
        (_ : header : xs) ->
            let headers   = mapMaybe getHeader header
                (ac, mcs) = L.mapAccumR (getRow headers) [] xs
                courses   = catMaybes mcs
            in Just $ Table headers courses
        _ -> Nothing

getHeader :: Cursor -> Maybe Header
getHeader = go . normalize . head . ($// content)
    where go "" = Nothing
          go x = Just x

-- | A row is either a category or course
getRow :: [Header] -> [Category] -> [Cursor] -> ([Category], Maybe Course)
getRow hs cats = go . map ($// content) -- ($// content)
    where go :: [ [Text] ] -> ([Category], Maybe Course)
          go (mc : vs) = case head mc of
            "\160"  -> (cats, Just $ toCourse cats hs $ map head vs)
            cat     -> (accumCategory cat cats, Nothing)

toCourse :: [Category] -> [Header] -> [Text] -> Course
toCourse cats hs xs = (cats, Map.fromList $ zip hs xs)

normalize = T.unwords . T.words . T.unlines . T.lines
