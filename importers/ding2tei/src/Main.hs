{-
 - Main.hs - main program
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


module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (die)

import App.Ding2TEI (ding2tei)
import Language.Ding.AlexScanner (scan)
import Language.Ding.Inverse (inverse)
import Language.Ding.Parser (parse)
import Language.TEI.ToXML (prettyTEI)
import Language.TEI.ToXML.ValidateChar (validateString)


-- Notes:
--  * The input file should be newline terminated, even though in most cases
--    it will not matter.
--    * One might consider adding a final newline, if there is none.
--    * Alex needs terminating whitespace, since the end of input  is not
--      treated as '\n' in right context (unlike the beginning of input).
--      (The whitespace in right context is needed to differentiate different
--      kinds of slashes.)


main :: IO ()
main = do
  (inFile, outFile, inverseFlag) <- getArgs >>= parseArgs

  input <- readFile inFile

  let ding = parse $ scan $ validateString input
  let ding' = if inverseFlag then inverse ding else ding
  let tei = ding2tei ding'
  writeFile outFile $ prettyTEI tei


parseArgs :: [String] -> IO (String, String, Bool)
parseArgs ["-i", inFile, outFile] = return (inFile, outFile, True)
parseArgs [inFile, outFile]       = return (inFile, outFile, False)
parseArgs _                       =
  die "Syntax: ding2tei [-i] <in_file> <out_file>"


-- vi: ft=haskell ts=2 sw=2 et
