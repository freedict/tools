{-
 - Language/TEI/Syntax/Reference.hs - references
 -
 - Copyright 2020,2022 Einhard Leichtfuß
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
 - TEI references, corresponding to the `xr' and `ref' XML tags.
 -}
module Language.TEI.Syntax.Reference
  ( Ident(..)
  , ReferenceGroup(..)
  , Reference(..)
  , RefType(..)
  ) where


import Data.List.NonEmpty (NonEmpty)


-- | An identifier representing `HW.n' where `HW' is a headword and `n'
--   a distinguishing number.
data Ident = Ident String Int
 deriving (Show, Eq, Ord)


-- | A list of references, sharing a type; corresponds to `xr' in TEI XML.
data ReferenceGroup
  = ReferenceGroup
      RefType
      (NonEmpty Reference)
 deriving (Show, Eq, Ord)


-- | A reference; corresponds to `ref' in TEI XML.
data Reference
  = Reference
      (Maybe Ident)   -- ^ target identifier
      String          -- ^ target string representation
 deriving (Show, Eq, Ord)


-- | Reference types, as describe in the FreeDict HowTo:
--   <https://github.com/freedict/fd-dictionaries/wiki/FreeDict-HOWTO-%E2%80%93-Writing-Text-Encoding-Initiative-XML-Files>
data RefType
  = Synonymy
  | Etymology     -- ^ not present in the Ding
  | Comparison    -- ^ not present in the Ding
  | Illustration  -- ^ not present in the Ding
  | Related
 deriving (Eq, Ord)


-- | Show reference types as recommended by FreeDict.
instance Show RefType where
  show Synonymy     = "syn"
  show Etymology    = "etym"
  show Comparison   = "cf"
  show Illustration = "illus"
  show Related      = "see"


-- vi: ft=haskell ts=2 sw=2 et
