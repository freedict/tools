{-
 - Data/NatLang/Grammar.hs - Grammar (linguistics) information
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
 - Grammar information (linguistics), as present in the Ding dictionary,
 - albeit also representable in TEI.
 -}
module Data.NatLang.Grammar
  ( GrammarInfo(..)
  , GramLexCategory(..)
  , PartOfSpeech(..)
  , VerbType(..)
  , PronounType(..)
  , Gender(..)
  , Number(..)
  , Case(..)
  , Collocate(..)
  ) where


import Data.NatLang.Usage (Usage)


-- | A grammar single annotation, where "{f,n}" counts as two of them.
data GrammarInfo
  = GramLexCategory GramLexCategory
  | Collocate       Collocate       [Usage]
 deriving (Show, Eq, Ord)


-- | Grammatical or lexical category (resp. values thereof).
--   See https://en.wikipedia.org/wiki/Grammatical_category for a distinction.
--   Might contain information not considered related to grammatical or lexical
--   categories.  I am not a linguist.
data GramLexCategory
  = PartOfSpeech    PartOfSpeech
  | Gender          Gender
  | Number          Number
  | Case            Case    -- ^ rare in the Ding
 deriving (Show, Eq, Ord)


data PartOfSpeech
  = Noun
  | Verb [VerbType]
  | Adjective
  | Adverb
  | Preposition
  | Conjunction
  | Article
  | Pronoun [PronounType]
  | Numeral
  | Interjection
 deriving (Show, Eq, Ord)

data VerbType
  = Transitive
  | Intransitive
  | Reflexive
 deriving (Show, Eq, Ord)

-- | Some pronoun types.
--   There are many more, but the Ding does not annotate them.
data PronounType
  = Personal
  | Interrogative
  | Relative
 deriving (Show, Eq, Ord)


data Gender
  = Feminine
  | Masculine
  | Neuter
 deriving (Show, Eq, Ord)


-- | Grammatical number
--   The argument to both constructors specifies whether the annotated word
--   may only occur in the respective number, that is, whether it is a
--   singulare/plurale tantum.
data Number
  = Singular Bool
  | Plural   Bool
 deriving (Show, Eq, Ord)


-- | Grammatical case.
--   Only those listed that appear in annotations in the Ding.
data Case
  = Genitive
  | Accusative
  | Dative
 deriving (Show, Eq, Ord)


-- | Grammar collocate, such as "{+Dat.}" or "{+conj}".
data Collocate
  = CollocCase
      [String]                      -- ^ interrogative pronouns; often none
      Case                          -- ^ case of collocating word
  | CollocPOS       PartOfSpeech    -- ^ POS of collocating word; rare
 deriving (Show, Eq, Ord)


-- vi: ts=2 sw=2 et
