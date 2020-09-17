{-
 - Test.hs - manual testing
 -
 - Copyright 2020 Einhard Leichtfuß
 -
 - This file is part of ding2tei-haskell.
 -
 - ding2tei-haskell is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Affero General Public License as published
 - by the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - ding2tei-haskell is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Affero General Public License for more details.
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with ding2tei-haskell.  If not, see <https://www.gnu.org/licenses/>.
 -}


module Test () where

import App.Ding2TEI
import Language.Ding.AlexScanner (scan)
import Language.Ding.Parser (parse)
import Language.Ding.Parser.Header (parseHeader)
import Language.Ding.Parser.Line (parseLine)
import Language.Ding.Pretty
import Language.Ding.Syntax
import Language.Ding.Token
import Language.Ding.Enrich
import Language.TEI.Syntax.Body
import Language.TEI.ToXML

import Data.NatLang.Dictionary
import Language.Ding.Enrich.Example

-- | For testing purposes, parse all but the first `n' lines.
--   Useful to identify several syntax errors in succession (avoids re-parsing
--   the known-good part).
--   A header is prepended.  Make sure to at least drop the header.
tailParse :: String -> Int -> IO ()
tailParse fileName n = do
  input <- readFile fileName
  let ding = parse $ scan $ header ++ (unlines $ drop n $ lines input)
  putStrLn $ show $ length $ show ding

header :: String
header = unlines
  [ "# Version :: 1.8.1 2016-09-06"
  , "# Copyright (c) :: Frank Richter <frank.richter.tu-chemnitz.de>,"
  , "# 1995 - 2016"
  , "# License :: GPL Version 2 or later; GNU General Public License"
  , "# URL :: http://dict.tu-chemnitz.de/"
  ]

example1 :: String
example1 =
  "Bäckerin {f}; Bäcker {m} | Bäckerei {f}; Backstube {f} :: baker | bakery\n"

example2 :: String
example2 = unlines
  [ "Wetter {n}; Witterung {f} :: weather"
  , "  Witterungen {pl} :: weathers"
  , "  bei jeder Witterung; bei jedem Wetter :: in all weathers"
  ]   -- from https://dict.tu-chemnitz.de/doc/syntax.html

example3 :: String
example3 = unlines
  [ "Whist {n} (Kartenspiel) :: whist"
  , "(Schaden; Mangel) beheben; (Missstand) abstellen; abhelfen; in Ordnung bringen :: to remedy "
  ]   -- from https://dict.tu-chemnitz.de/doc/syntax.html

-- vi: ft=haskell ts=2 sw=2 et
