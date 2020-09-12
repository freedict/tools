{-
 - Data/NatLang/Language.hs - specific languages
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


module Data.NatLang.Language
  ( Language(..)
  , showCode
  ) where


-- | There are more languages.  They are not of particular importance for the
--   German-English Ding dictionary though.
data Language = German | English
  deriving (Eq, Ord)


-- | Show the full language name, in English.
instance Show Language where
  show German = "German"
  show English = "English"


-- | Show short language code, as to be used in @xml:lang.
--   This is the ISO 639-1 code, unless no such one is available, where the
--   ISO 639-3 code is shown.
showCode :: Language -> String
showCode German  = "de"
showCode English = "en"


-- vi: ft=haskell ts=2 sw=2 et
