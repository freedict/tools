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


{-|
 - Manual testing.  No function exported.  To be used in `ghci' exclusively.
 -
 - Re-exports many data structures and functions.
 -}
module Test () where

import Control.Monad.Trans.Writer (runWriter)

import App.Ding2TEI
import Data.NatLang.Dictionary
import Data.NatLang.Grammar
import Language.Ding.AlexScanner (scan)
import Language.Ding.Parser (parse)
import Language.Ding.Parser.Header (parseHeader)
import qualified Language.Ding.Parser.Line as LP (parseLine)
import Language.Ding.Pretty
import Language.Ding.Syntax
import Language.Ding.Token
import Language.Ding.Enrich
import Language.TEI.Syntax
import Language.TEI.ToXML

-- | For testing purposes, parse all but the first `n' lines.
--   Only print the number of lines parsed.
--   Useful to identify several syntax errors in succession (avoids re-parsing
--   the known-good part).
--   A header is prepended.  Make sure to at least drop the header.
tailParse :: String -> Int -> IO ()
tailParse fileName n = do
  input <- readFile fileName
  let ding = parse $ scan $ header ++ (unlines $ drop n $ lines input)
  putStrLn $ show $ length $ show ding

parseLine :: [Token] -> Line
parseLine = fst . runWriter . LP.parseLine

header :: String
header = unlines
  [ "# Version :: 1.8.1 2016-09-06"
  , "# Copyright (c) :: Frank Richter <frank.richter.tu-chemnitz.de>,"
  , "# 1995 - 2016"
  , "# License :: GPL Version 2 or later; GNU General Public License"
  , "# URL :: http://dict.tu-chemnitz.de/"
  ]


-- | Some example lines; all except the first from
--   <https://dict.tu-chemnitz.de/doc/syntax.html>.
examples :: [String]
examples =
  [ unwords
      [ "Bäckerin {f}; Bäcker {m} | Bäckerei {f}; Backstube {f}"
      , ":: baker | bakery\n"
      ]
  , unwords
      [ "Wetter {n}; Witterung {f} | Witterungen {pl} | bei jeder Witterung;"
      , "bei jedem Wetter :: weather | weathers | in all weathers\n"
      ]
    -- Outdated syntax:
    --unlines
    --  [ "Wetter {n}; Witterung {f} :: weather"
    --  , "  Witterungen {pl} :: weathers"
    --  , "  bei jeder Witterung; bei jedem Wetter :: in all weathers"
    --  ]
  , "Whist {n} (Kartenspiel) :: whist\n"
  , unwords
      [ "(Schaden; Mangel) beheben; (Missstand) abstellen; abhelfen;"
      , "in Ordnung bringen :: to remedy \n"
      ]
  ]

-- vi: ft=haskell ts=2 sw=2 et
