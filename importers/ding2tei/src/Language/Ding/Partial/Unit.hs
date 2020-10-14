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


{-|
 - Intermediate representation and handling of partially parsed units.
 -}
module Language.Ding.Partial.Unit
  ( PartialUnit
  , fromToken
  , fromVerbToken
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
import qualified Data.List.NonEmpty as NEList (toList)

import Data.NatLang.Grammar
  ( GrammarInfo(GramLexCategory)
  , GramLexCategory(PartOfSpeech)
  , PartOfSpeech(Verb)
  )
import Data.NatLang.InflectedForms (InflectedForms)
import Data.NatLang.Usage (Usage)
import Language.Ding.Syntax (Unit(..))
import Language.Ding.Token (Token, tokenToString)


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

-- | Partial unit that may be expanded with annotations and regular tokens,
--   and finally converted into a `Language.Ding.Syntax.Unit'.
--   All fields' contents are stored in reverse order.
data PartialUnit = PartialUnit
  { headwordToks :: [Token]       -- ^ tokens to later form the headword
  , plainToks :: [Token]          -- ^ tokens to form a potential example
  , gramAnnots :: [NonEmpty GrammarInfo]
  , usageAnnots :: [NonEmpty Usage]
  , abbrevAnnots :: [NonEmpty String]
  , mInfl :: Maybe InflectedForms
  , suffixes :: [String]
  , references :: [String]
  }


empty :: PartialUnit
empty = PartialUnit [] [] [] [] [] Nothing [] []

-- | Convert a token to a partial unit without any annotation.
fromToken :: Token -> PartialUnit
fromToken = (empty `plusToken`)

-- | Like fromToken, but mark as verb.  Meant to be used for units prefixed by
--   "to".
fromVerbToken :: Token -> PartialUnit
fromVerbToken t =
  fromToken t `plusGramAnnot` pure (GramLexCategory $ PartOfSpeech $ Verb [])

-- | Add a token.
--   All previous annotations except parenthesis expression are thrown away.
--   (Due to the presence of the processed token, they are infix
--   annotations.)
--   Preceding parenthesis expressions are treated equal to preceding text
--   tokens, and therefore effectively retained in verbatim.
plusToken :: PartialUnit -> Token -> PartialUnit

-- Use plainToks also for the headwordToks, because they contain literal
-- ()-annots.
plusToken pu t = empty
  { headwordToks = toks'
  , plainToks    = toks'
  }
 where toks' = t : plainToks pu

plusGramAnnot :: PartialUnit -> NonEmpty GrammarInfo -> PartialUnit
plusGramAnnot pu as = pu { gramAnnots = as : gramAnnots pu }

plusUsageAnnot :: PartialUnit -> NonEmpty Usage -> PartialUnit
plusUsageAnnot pu as = pu { usageAnnots = as : usageAnnots pu }

-- Note:
--  * One might consider keeping one of the abbreviations in the example
--    version (i.e., as Token) to allow for abbreviations to be retained
--    verbatim when infixes of an example unit.
--    * Further analysis required (TODO).
plusAbbrevAnnot :: PartialUnit -> NonEmpty String -> PartialUnit
plusAbbrevAnnot pu as = pu { abbrevAnnots = as : abbrevAnnots pu }

-- | Add an inflection annotation.
--   If there are added several ones in succession, the last one is prefered.
plusInflAnnot :: PartialUnit -> InflectedForms -> PartialUnit
plusInflAnnot pu infl = pu { mInfl = Just infl }

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
--   Takes a list of (`String', `Token') prefixes that stem from parenthesis
--   expressions, where the type has the same significance as in `plusSuffix'.
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
  , unitInflected  = mInfl pu
  , unitReferences = reverse $ references pu
  , unitExamples   = []   -- populated during enrichment
  }
 where
  (prefStrings, prefToks) = unzip prefs


-- vi: ft=haskell ts=2 sw=2 et
