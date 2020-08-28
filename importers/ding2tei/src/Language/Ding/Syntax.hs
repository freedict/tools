{-
 - Language/Ding/Syntax.hs - general AST structures
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

module Language.Ding.Syntax
  ( Dict(..)
  , Header(..)
  , Line(..)
  , Entry(..)
  , Group(..)
  , Unit(..)
  , Annotation(..)
  ) where

import Language.Ding.Syntax.Grammar (GrammarAnnotation)

-- | A whole dictionary (set of German-English translation pairs)
data Dict = Dict Header [Line]
 deriving Show

-- | The header of the Ding.
data Header = Header
  { headerVersion         :: String
  , headerVersionDate     :: String
  , headerCopyrightHolder :: String
  , headerCopyrightPeriod :: String
  , headerLicense         :: String
  , headerURL             :: String
  }
 deriving Show

-- | A set of related entries.
data Line = Line [Entry]
 deriving Show

-- Two lines may be joined by just concatenating the entries.  This is useful
-- for line continuations.
instance Semigroup Line where
  Line a <> Line b = Line $ a <> b

instance Monoid Line where
  mempty = Line mempty

-- | A pair of corresponding `Group's.
--   The order of languages is that of the input, so German first, than
--   English.
data Entry = Entry Group Group
 deriving Show

-- | A set of `Unit's matching the same translation.
data Group = Group [Unit]
 deriving Show

-- | A single (key-)word or phrase, in one language, with annotations.
data Unit = Unit String [GrammarAnnotation] -- TODO: other annotations
          | NullUnit      -- empty
          -- | AbbrevUnit ...
 deriving Show

data Annotation
  = GramAnnot GrammarAnnotation
  | MiscAnnot String    -- TODO
 deriving Show

-- vi: ts=2 sw=2 et
