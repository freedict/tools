{-
 - Language/Ding/Inverse.hs - Invert the language order
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

module Language.Ding.Inverse (inverse) where

import Data.NatLang.Dictionary
import Language.Ding.Syntax


-- | Give the inverse of a dictionary (invert the order of languages).
--   This function is bijective and its own inverse.
inverse :: Ding -> Ding
inverse (Dictionary header srcLang tgtLang (Body ls)) =
  Dictionary header tgtLang srcLang (Body $ map lineInverse ls)

lineInverse :: Line -> Line
lineInverse (Line entries) = Line $ map entryInverse entries

entryInverse :: Entry -> Entry
entryInverse (Entry g h) = Entry h g


-- vi: ft=haskell ts=2 sw=2 et
