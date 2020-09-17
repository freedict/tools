{-
 - Language/Common/Syntax.hs - common syntax elements
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
 - common syntax elements of both Ding and TEI, exlcluding those in
 - `Data.NatLang'.
 -}
module Language.Common.Syntax
  ( Example(..)
  ) where


-- | An example in the source language with translation.
--   Usually, there is one translation given, however any number is possible
--   (including none).
data Example
  = Example
      String    -- ^ example in the source language
      [String]  -- ^ set of translations
 deriving (Show, Eq, Ord)


-- vi: ft=haskell ts=2 sw=2 et
