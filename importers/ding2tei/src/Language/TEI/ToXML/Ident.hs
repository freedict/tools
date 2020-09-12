{-
 - Language/TEI/ToXML/Ident.hs - encode xml:id's
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

module Language.TEI.ToXML.Ident
  ( identToXMLId
  ) where

import Data.Char (ord)

import Language.TEI.Syntax.Reference (Ident(Ident))


-- References:
--  * https://www.w3.org/TR/xmlschema-2/#ID | #NCName
--      / https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#ID | #NCName
--  * https://www.w3.org/TR/xml-names/#NT-NCName
--      / https://www.w3.org/TR/2009/REC-xml-names-20091208/#NT-NCName
--    > NCName ::= Name - (Char* ':' Char*) /* An XML Name, minus the ":" */
--  * https://www.w3.org/TR/xml/#NT-Name
--      / https://www.w3.org/TR/2008/REC-xml-20081126/#NT-Name
--    > Name ::= NameStartChar (NameChar)*
--  * https://www.w3.org/TR/xml/#NT-NameStartChar | #NT-NameChar
--      / http://www.w3.org/TR/2008/REC-xml-20081126/
--    * defines NameStartChar and NameChar
--    * Deduce: NCNameStartChar and NCNameChar (- ':')


-- | Translate an `Ident' to a legal @xml:id (NCName).
--   The identifier's number is appended dot-separated to avoid ambiguity.
identToXMLId :: Ident -> String
identToXMLId (Ident s n) = encodeNCName s ++ '.' : show n


-- Use '_' as escape char, since it is legal everywhere.
encodeNCName :: String -> String
encodeNCName ""     = "_empty_"
encodeNCName (c:cs) = encodeNCNameStartChar c ++ concatMap encodeNCNameChar cs

encodeNCNameStartChar :: Char -> String
encodeNCNameStartChar ' ' = "__"
encodeNCNameStartChar c
  | isNCNameStartChar c && c /= '_' = pure c
  | otherwise                       = escapeChar c

encodeNCNameChar :: Char -> String
encodeNCNameChar ' ' = "__"
encodeNCNameChar c
  | isNCNameChar c && c /= '_' = pure c
  | otherwise                  = escapeChar c

escapeChar :: Char -> String
escapeChar c = '_' : show (ord c) ++ "_"


isNCNameStartChar :: Char -> Bool
isNCNameStartChar c =
     'A' <= c && c <= 'Z'
  || c == '_'
  || 'a'<= c && c <= 'z'
  || '\xC0' <= c && c <= '\xD6'
  || '\xD8' <= c && c <= '\xF6'
  || '\xF8' <= c && c <= '\x2FF'
  || '\x370' <= c && c <= '\x37D'
  || '\x37F' <= c && c <= '\x1FFF'
  || '\x200C' <= c && c <= '\x200D'
  || '\x2070' <= c && c <= '\x218F'
  || '\x2C00' <= c && c <= '\x2FEF'
  || '\x3001' <= c && c <= '\xD7FF'
  || '\xF900' <= c && c <= '\xFDCF'
  || '\xFDF0' <= c && c <= '\xFFFD'
  || '\x10000' <= c && c <= '\xEFFFF'

isNCNameChar :: Char -> Bool
isNCNameChar c =
     isNCNameStartChar c
  || c == '-'
  || c == '.'
  || '0' <= c && c <= '9'
  || c == '\xB7'
  || '\x0300' <= c && c <= '\x036F'
  || '\x203F' <= c && c <= '\x2040'


-- vi: ft=haskell ts=2 sw=2 et
