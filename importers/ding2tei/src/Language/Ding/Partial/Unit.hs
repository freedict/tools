{-
 - Language/Ding/Partial/Unit.hs - partial units
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


module Language.Ding.Partial.Unit
  ( PartialUnit
  , fromToken
  , toUnit
  , plusToken
  , plusGramAnnot
  , plusUsageAnnot
  , plusAbbrevAnnot
  , plusInflAnnot
  , plusSuffix
  , plusRef
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import Data.Maybe (listToMaybe)

import Data.NatLang.GrammarInfo
import Data.NatLang.InflectedForms
import Data.NatLang.Usage
import Language.Ding.Syntax
import Language.Ding.Token


-- Notes
--  * A PartialUnit contains information on a prefix of a unit.
--    * It can be "modified" by appending annotations and regular unit text.
--      * See below on "Modification".
--  * "Modification": Inefficiency
--    * A PartialUnit is immutable, hence any operation that might look like
--      an update actually creates a new PartialUnit.
--      * Cost is proportional to the number of fields in PartialUnit.
--    * The problem could potentially be avoided by changing PartialUnit to
--      essentially be a list of a sum type of all potential elements.  This
--      list would then later be processed by means equivalent to `plus*'.
--  * All `plus*' functions are meant to be used in infix-form.

-- TODO?: polymorphic data type joining PartialUnit and Unit.

-- | Partial unit that may be expanded with annotations and regular tokens,
--   and finally converted into a `Language.Ding.Syntax.Unit'.
--   All fields are stored in reverse order.
data PartialUnit = PartialUnit
  { headwordToks :: [Token]       -- ^ tokens to later form the headword
  , plainToks :: [Token]          -- ^ tokens to form a potential example
  , gramAnnots :: [NonEmpty GrammarInfo]
  , usageAnnots :: [NonEmpty Usage]
  , abbrevAnnots :: [NonEmpty String]
  , infls :: [InflectedForms]
  , suffixes :: [String]
  , references :: [String]
  }


empty :: PartialUnit
empty = PartialUnit [] [] [] [] [] [] [] []

fromToken :: Token -> PartialUnit
fromToken = (empty `plusToken`)

plusToken :: PartialUnit -> Token -> PartialUnit
plusToken pu t = empty
  { headwordToks = t : headwordToks pu
  , plainToks    = t : plainToks pu
  }

plusGramAnnot :: PartialUnit -> NonEmpty GrammarInfo -> PartialUnit
plusGramAnnot pu as = pu { gramAnnots = as : gramAnnots pu }

plusUsageAnnot :: PartialUnit -> NonEmpty Usage -> PartialUnit
plusUsageAnnot pu as = pu { usageAnnots = as : usageAnnots pu }

-- Note:
--  * One might consider keeping one of the abbreviations in the example
--    version.
--    * Further analysis required (TODO).
plusAbbrevAnnot :: PartialUnit -> NonEmpty String -> PartialUnit
plusAbbrevAnnot pu as = pu { abbrevAnnots = as : abbrevAnnots pu }

plusInflAnnot :: PartialUnit -> InflectedForms -> PartialUnit
plusInflAnnot pu infl = pu { infls = infl : infls pu }

-- | Takes both the string representing the suffix, without potential
--   enclosing parentheses, and a token that includes potential parentheses.
plusSuffix :: PartialUnit -> (String, Token) -> PartialUnit
plusSuffix pu (s, t) = pu
  { suffixes  = s : suffixes pu
  , plainToks = t : plainToks pu
  }

plusRef :: PartialUnit -> String -> PartialUnit
plusRef pu ref = pu { references = ref : references pu }


-- | From a list of prefixes and a partial unit, construct a unit.
--   Takes a list of (String, Token), with the same meaning as in plusSuffix.
toUnit :: [(String, Token)] -> PartialUnit -> Unit
toUnit prefs pu = Unit
  { unitHeadword   = tokenToString $ mconcat $ reverse $ headwordToks pu
  , unitPlain      = tokenToString $ mconcat $
                       prefToks ++ (reverse $ plainToks pu)
  , unitGrammar    = concat $ reverse $ map NEList.toList $ gramAnnots pu
  , unitUsages     = concat $ reverse $ map NEList.toList $ usageAnnots pu
  , unitPrefixes   = prefStrings
  , unitSuffixes   = reverse $ suffixes pu
  , unitAbbrevs    = concat $ reverse $ map NEList.toList $ abbrevAnnots pu
  , unitInflected  = listToMaybe $ infls pu    -- extract the last one, if any
  , unitReferences = reverse $ references pu
  }
 where
  (prefStrings, prefToks) = unzip prefs


-- vi: ft=haskell ts=2 sw=2 et
