{-
 - Language/Ding/Partial/PseudoUnit.hs - partial pseudo units
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
 - Handling of syntax elements that syntactically look like units however
 - only contain annotations, which are to be applied to preceding units.
 -}
module Language.Ding.Partial.PseudoUnit
  ( PartialPseudoUnit
  , fromSuffixes
  , addToUnit
  , plusGramAnnot
  , plusUsageAnnot
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList

import Data.NatLang.Grammar (GrammarInfo)
import Data.NatLang.Usage (Usage)
import Language.Ding.Syntax (Unit(..))
import Language.Ding.Token (Token, tokenToString)

-- | Partial pseudo unit that may be expanded with annotations and finally
--   adjuncted to a unit.
data PartialPseudoUnit = PartialPseudoUnit
  { plainToks :: [Token]
  , suffixes :: [String]
  , gramAnnots :: [NonEmpty GrammarInfo]
  , usageAnnots :: [NonEmpty Usage]
  }


empty :: PartialPseudoUnit
empty = PartialPseudoUnit [] [] [] []

-- | Create a `PartialPseudoUnit' from a list of parenthesis expressions.
fromSuffixes :: [(String, Token)] -> PartialPseudoUnit
fromSuffixes sufs = empty
  { suffixes  = map fst sufs
  , plainToks = map snd sufs
  }

plusGramAnnot :: PartialPseudoUnit -> NonEmpty GrammarInfo -> PartialPseudoUnit
plusGramAnnot psu as = psu { gramAnnots = as : gramAnnots psu }

plusUsageAnnot :: PartialPseudoUnit -> NonEmpty Usage -> PartialPseudoUnit
plusUsageAnnot psu as = psu { usageAnnots = as : usageAnnots psu }

-- | Add all annotations contained in a partial pseudo unit to a regular unit.
addToUnit :: PartialPseudoUnit -> Unit -> Unit

-- Note:
--  * Due to the use of (++), this is not very efficient (for larger lists).
--    * This could be improved by applying to a PartialUnit instead, which has
--      stored its elements in inverse order.
--      * This would however likely render the HappyParser.y code less
--        readable.
addToUnit psu u = u
  { unitPlain    =
      unitPlain u ++ tokenToString (mconcat $ reverse $ plainToks psu)
  , unitSuffixes = unitSuffixes u ++ suffixes psu
  , unitGrammar  =
      unitGrammar u ++ concat (reverse $ map NEList.toList $ gramAnnots psu)
  , unitUsages   =
      unitUsages u ++ concat (reverse $ map NEList.toList $ usageAnnots psu)
  }


-- vi: ft=haskell ts=2 sw=2 et
