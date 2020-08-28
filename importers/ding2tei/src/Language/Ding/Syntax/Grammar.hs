{-
 - Language/Ding/Syntax/Grammar.hs - Grammar Annotations
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


module Language.Ding.Syntax.Grammar
  ( GrammarAnnotation(..)
  , GrammaticalNumber(..)
  , SingulareTantum(..)
  , PluraleTantum(..)
  , PartOfSpeech(..)
  , VerbType(..)
  , Gender(..)
  , grammarMap
  , grammarMapRev
  ) where


-- According to the documentation, `Data.Map.Strict' is prefered over
-- `Data.Map.Lazy' in this case (as in many).
-- see: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map.html
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)


-- Note: The only annotated colloc-POS is "{+conj}".
data GrammarAnnotation = GramNum GrammaticalNumber
                       | POS PartOfSpeech
                       | CollocCase [InterrogPron] Case
                         -- ^ case of collocating word
                       | CollocPOS PartOfSpeech
 deriving (Show, Eq, Ord)


data GrammaticalNumber = Singular (Maybe SingulareTantum)
                       | Plural (Maybe PluraleTantum)
 deriving (Show, Eq, Ord)

-- | A singulare tantum is a word that occurs only in the singular form
data SingulareTantum = SingulareTantum
 deriving (Show, Eq, Ord)

-- Note: There is no single annotation for pluralia tantum.
-- | A plurale tantum is a word that occurs only in the plural form
data PluraleTantum = PluraleTantum
 deriving (Show, Eq, Ord)


data PartOfSpeech = Verb (Maybe VerbType)
                  | Noun Gender
                  | Adjective
                  | Adverb
                  | Proposition             -- often accompanied by +Gen/+Dat
                                            --  -> TODO
                  | Conjugation
                  | Article
                  | PersonalPronoun GrammaticalNumber
                                            -- TODO: NoPlural is impossible
                  | Numeral
 deriving (Show, Eq, Ord)


data VerbType = Transitive
              | Intransitive
              | Reflexive
 deriving (Show, Eq, Ord)


data Gender = Feminine
            | Masculine
            | Neuter
 deriving (Show, Eq, Ord)


-- | Grammatical case.  Only those listed that appear in annotations in the
--   Ding.
data Case = Genitive
          | Accusative
          | Dative
 deriving (Show, Eq, Ord)


data InterrogPron = InterrogPron String
 deriving (Show, Eq, Ord)

interrogProns :: Set String
interrogProns = Set.fromList
  [ "wo?"
  , "wohin?"
  , "wann?"
  , "bis wann?"
  ]


-- Note: Most grammar annotations occur only on the german side.
-- | map from brace-enclosed keys to `GrammarAnnotation's
grammarMap :: Map String GrammarAnnotation
grammarMap = Map.fromList grammarListMap

-- | map from GrammarAnnotation's to their string representation
grammarMapRev :: Map GrammarAnnotation String
grammarMapRev = Map.fromList $ map swap grammarListMap

grammarListMap :: [(String, GrammarAnnotation)]
grammarListMap =
  [ ("sing"     , GramNum $ Singular Nothing)     -- rare
  , ("pl"       , GramNum $ Plural Nothing)
  , ("no pl"    , GramNum $ Singular $ Just SingulareTantum)  -- rare
  , ("no sing"  , GramNum $ Plural   $ Just PluraleTantum)    -- rare
  , ("v"        , POS $ Verb Nothing)
  , ("vt"       , POS $ Verb $ Just Transitive)
  , ("vi"       , POS $ Verb $ Just Intransitive)
  , ("vr"       , POS $ Verb $ Just Reflexive)
  , ("f"        , POS $ Noun Feminine)
  , ("m"        , POS $ Noun Masculine)
  , ("n"        , POS $ Noun Neuter)
  , ("adj"      , POS Adjective)
  , ("adv"      , POS Adverb)
  , ("prp"      , POS Proposition)
  , ("conj"     , POS Conjugation)
  , ("art"      , POS Article)                    -- rare
  , ("ppron"    , POS $ PersonalPronoun $ Singular Nothing) -- rare
  , ("ppron pl" , POS $ PersonalPronoun $ Plural Nothing)   -- rare
  , ("num"      , POS Numeral)
  ]

-- | map from a case's abbreviated string represenation to Case
caseMap :: Map String Case
caseMap = Map.fromList caseListMap

-- | map from Case to the corresponding abbreviated string representation
caseMapRev :: Map Case String
caseMapRev = Map.fromList $ map swap caseListMap

-- Note: The abbreviated annotations stem from german words.  Hence "Akk.".
caseListMap :: [(String, Case)]
caseListMap =
  [ ("Gen.", Genitive)
  , ("Akk.", Accusative)
  , ("Dat.", Dative)
  ]

-- vi: ts=2 sw=2 et
