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

import Language.Ding.AlexScanner (scan)
import Language.Ding.HappyParser (parse)
import Language.Ding.Syntax (Dict(..))


main :: IO ()
main = do
  args <- getArgs
  when (null args) $ die "Error: Missing argument (filename)"
  let fileName = head args
  input <- readFile fileName
  let Dict _ ls = parse $ scan input
  putStrLn $ show $ length ls


-- vi: ts=2 sw=2 et
