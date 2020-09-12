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
  ( grammarMap
  , grammarMapRev
  , caseMapRev
  , posMapRev
  , interrogProns
  ) where


-- According to the documentation, `Data.Map.Strict' is prefered over
-- `Data.Map.Lazy' in this case (as in many).
-- see: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map.html
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import Data.NatLang.Grammar


-- Notes:
--  * It would be more efficient to directly write functions instead of
--    relying on maps (dictionaries), since the former can do pattern matching.
--    * A similar effect could possibly be obtained by using tries (sic!).
--    * The rationale behing the current state is to avoid duplication (of
--      semantics in the code) - One might easily forget to update one of the
--      two functions on a change.


-- Note: Most grammar annotations occur only on the german side.
-- | map from brace-enclosed keys to `GramLexCategory's
grammarMap :: Map String GramLexCategory
grammarMap = Map.fromList grammarListMap

-- | map from `GramLexCategory's to their string representation
--   This map is partial.  Collocation information (`CollocCase',
--   `CollocPOS') are not in the domain.
--   -- TODO: Add this information to where this map is used.
grammarMapRev :: Map GramLexCategory String
grammarMapRev = Map.fromList $ map swap grammarListMap

-- Does not contain CollocCase and CollocPOS.  These are parsed differently.
-- In particular, they are not identified in the scanner.
grammarListMap :: [(String, GramLexCategory)]
grammarListMap =
  [ ("f"             , Gender Feminine)   -- implies noun, sg
  , ("m"             , Gender Masculine)  -- implies noun, sg
  , ("n"             , Gender Neuter)     -- implies noun, sg
  , ("sing"          , Number Singular)   -- rare
  , ("pl"            , Number Plural)
  , ("no pl"         , SingulareTantum)   -- rare; implies noun, sg
  , ("no sing"       , PluraleTantum)     -- rare; implies noun, pl
  ]
    ++ map (\ (s, pos) -> (s, PartOfSpeech pos)) posListMap
    ++ map (\ (s, cas) -> (s, Case cas)) caseListMap        -- rare


-- Not yet used (TODO: remove?).
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


posMapRev :: Map PartOfSpeech String
posMapRev = Map.fromList $ map swap posListMap

-- Needed separately, because also occuring as `CollocCase'.
posListMap :: [(String, PartOfSpeech)]
posListMap =
  [ ("v"             , Verb [])
  , ("vt"            , Verb [Transitive])
  , ("vi"            , Verb [Intransitive])
  , ("vr"            , Verb [Reflexive])
  , ("adj"           , Adjective)
  , ("adv"           , Adverb)
  , ("prp"           , Preposition)
  , ("conj"          , Conjunction)
  , ("art"           , Article)                     -- rare
  , ("pron"          , Pronoun [])
  , ("ppron"         , Pronoun [Personal])       -- rare
  , ("pron interrog" , Pronoun [Interrogative])  -- rare
  , ("pron relativ"  , Pronoun [Relative])       -- rare
  , ("num"           , Numeral)
  , ("interj"        , Interjection)
  ]


interrogProns :: Set String
interrogProns = Set.fromList
  [ "wo?"
  , "wohin?"
  , "wann?"
  , "bis wann?"
  ]


-- vi: ts=2 sw=2 et
