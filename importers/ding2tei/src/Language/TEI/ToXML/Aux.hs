{-
 - Language/TEI/ToXML/Aux.hs - auxiliary functions for XML generation
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


module Language.TEI.ToXML.Aux
  ( uattr
  , xmlLangAttr
  , xmlIdAttr
  , text
  , mergeContent
  ) where


import Text.XML.Light


-- | Attribute with unqualified name, analogous to `unode'.
uattr :: String -> String -> Attr
uattr k v = Attr (unqual k) v

-- | @xml:lang attribute
xmlLangAttr :: String -> Attr
xmlLangAttr lang = Attr (QName "lang" Nothing (Just "xml")) lang

-- | @xml:id attribute
xmlIdAttr :: String -> Attr
xmlIdAttr val = Attr (QName "id" Nothing (Just "xml")) val

-- | Text content.  Upon pretty printing, special XML characters are escaped.
text :: String -> Content
text s = Text $ CData CDataText s Nothing

-- | Combine adjacent text and elements into the correspondonding string,
--   wrapped as a single `Content'.
--   This is an ugly workaround to the awkward "pretty" formatting of
--   `Text.XML.Light', where a list of `Content' (length >= 2) is always
--   converted to a corresponding list of idented lines.
mergeContent :: [Content] -> Content
mergeContent cs = Text $ CData CDataRaw rawString Nothing
 where
  rawString = concatMap (ppcContent defaultConfigPP) cs


-- vi: ft=haskell ts=2 sw=2 et
