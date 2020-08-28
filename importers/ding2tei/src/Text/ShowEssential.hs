{-
 - Text/ShowEssential.hs - A class to extract simple string representations
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

module Text.ShowEssential where

-- | A class for conversion to strings, only keeping essential information,
--   in a simple format.
--   Mostly used for simple structures.
class ShowEssential a where

  -- | Extract the essential information into a simple human readable string.
  showEssential :: a -> String

-- vi: sw=2 ts=2 et
