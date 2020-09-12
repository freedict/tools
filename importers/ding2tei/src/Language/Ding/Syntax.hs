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
  ( Ding
  , Header(..)
  , Line(..)
  , Entry(..)
  , Group(..)
  , Unit(..)
  ) where

import Data.NatLang.Dictionary (Dictionary)
import Data.NatLang.GrammarInfo (GrammarInfo)
import Data.NatLang.InflectedForms (InflectedForms)
import Data.NatLang.Usage (Usage)


-- | A whole dictionary (set of German-English translation pairs), together
--   with a header.
type Ding = Dictionary Header Line

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
newtype Line = Line [Entry]
 deriving Show

-- | A pair of corresponding `Group's.
--   Upon parsing from the Ding, the order of languages is the same as there.
--   The order may be flipped later though.
data Entry = Entry Group Group
 deriving (Show, Eq, Ord)

-- | A set of `Unit's matching the same translation.
--   It may be empty, in which a later created corresponding TEI entry may
--   have zero translations (if the `Group' is on the target language's side).
--   If an empty `Group' is on the source language's side, it is not at all
--   represented in the resulting TEI.
newtype Group = Group [Unit]
 deriving (Show, Eq, Ord)

-- TODO: ? Represent units being abbreviations differently?
--       - e.g.: add UnitType argument -- @type="abbrev"
-- | A single (key-)word or phrase, in one language, with annotations.
data Unit = Unit
  { unitHeadword   :: String
  , unitPlain      :: String        -- ^ includes literal <()>-annotations
  , unitGrammar    :: [GrammarInfo]
  , unitUsages     :: [Usage]
  , unitPrefixes   :: [String]
  , unitSuffixes   :: [String]
  , unitAbbrevs    :: [String]
  , unitInflected  :: Maybe InflectedForms
  , unitReferences :: [String]
  }
 deriving (Show, Eq, Ord)


-- vi: ft=haskell ts=2 sw=2 et
