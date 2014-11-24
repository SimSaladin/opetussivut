------------------------------------------------------------------------------
-- | 
-- Module         : N
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Main where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.HTML.DOM (praseLBS)
import Text.XML.Cursor (Cursor, attributIs, content, element, fromDocument,
                       child, ($//) (&|), (&//), (>=>))

url = "https://wiki.helsinki.fi/display/fysoppiaine/HY+fysiikan+oppiaineen+kurssisuunnitelma+s2014-k2016"

findTable = element "table"
