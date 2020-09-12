{-
 - Language/TEI/ToXML/ValidateChar.hs - validate character data
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


module Language.TEI.ToXML.ValidateChar
  ( validateString
  , validateChar
  ) where


-- | Validate that a string is valid in XML, as defined at
--   <https://www.w3.org/TR/xml/#charsets>.
--   Throws an error otherwise.
validateString :: String -> String
validateString = map validateChar

-- | Validate that a character is valid in XML, as defined at
--   <https://www.w3.org/TR/xml/#charsets>.
--   Throws an error otherwise.
validateChar :: Char -> Char
validateChar c =
  if c `elem` "\x9\xA\xD"
    || '\x20'    <= c && c <= '\xD7FF'
    || '\xE000'  <= c && c <= '\xFFFD'
    || '\x10000' <= c && c <= '\x10FFFF'
  then c
  else error $ "Invalid character in input: " ++ show c


-- vi: ft=haskell ts=2 sw=2 et
