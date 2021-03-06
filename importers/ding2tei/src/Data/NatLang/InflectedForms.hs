{-
 - Data/NatLang/InflectedForms.hs - information on inflected forms
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
 - Data types for inflected forms, as they appear in the Ding dictionary and
 - may be (partially) represented in TEI.
 -}
module Data.NatLang.InflectedForms
  ( InflectedForms(..)
  , InflectedForm(..)
  ) where


import Data.List.NonEmpty (NonEmpty)

import Data.NatLang.Usage (Usage)


-- | Two inflected forms of a word.  This is meant to be used for the English
--   language's simple past and past participle of a verb.
data InflectedForms
  = InflectedForms
      (NonEmpty InflectedForm)  -- ^ simple past
      (NonEmpty InflectedForm)  -- ^ past participle
 deriving (Show, Eq, Ord)


-- | A single inflected form, annotated with usages.
--   Note that these usages cannot be specified in TEI, or at least it is
--   unknown how.
data InflectedForm = InflectedForm String [Usage]
 deriving (Show, Eq, Ord)


-- vi: ft=haskell ts=2 sw=2 et
