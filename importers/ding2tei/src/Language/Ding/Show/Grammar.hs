{-
 - Language/Ding/Show/Grammar.hs - convert grammar annotations to strings
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
 - Convert grammar annotations to strings, as they might occur in the Ding
 - dictionary.  Note that they generally have a different representation in
 - TEI.
 -
 - The functions herin are both used by the pretty printer, but also by the
 - parser, to convert grammar keywords identified in the lexer back to their
 - string representation, when they do not have special meaning due to their
 - context--of which the lexer is unaware.
 -}
module Language.Ding.Show.Grammar
  ( showGLC
  , showPOS
  , showCase
  ) where

import Data.NatLang.Grammar


showGLC :: GramLexCategory -> [String]
showGLC (PartOfSpeech pos) = showPOS pos
showGLC (Gender gen)       = pure $ showGender gen
showGLC (Number num)       = pure $ showNumber num
showGLC (Case cas)         = pure $ showCase cas

showPOS :: PartOfSpeech -> [String]
showPOS Noun             = pure "noun"
showPOS (Verb [])        = pure "v"
showPOS (Verb vTypes)    = map showTypedVerb vTypes
showPOS Adjective        = pure "adj"
showPOS Adverb           = pure "adv"
showPOS Preposition      = pure "prp"
showPOS Conjunction      = pure "conj"
showPOS Article          = pure "art"
showPOS (Pronoun [])     = pure "pron"
showPOS (Pronoun pTypes) = map showTypedPronoun pTypes
showPOS Numeral          = pure "num"
showPOS Interjection     = pure "interj"

showTypedVerb :: VerbType -> String
showTypedVerb Transitive   = "vt"
showTypedVerb Intransitive = "vi"
showTypedVerb Reflexive    = "vr"

showTypedPronoun :: PronounType -> String
showTypedPronoun Personal      = "ppron"
showTypedPronoun Interrogative = "pron interrog"
showTypedPronoun Relative      = "pron relativ"

showGender :: Gender -> String
showGender Feminine  = "f"
showGender Masculine = "m"
showGender Neuter    = "n"

showNumber :: Number -> String
showNumber (Singular False) = "sing"
showNumber (Singular True)  = "no pl"
showNumber (Plural   False) = "pl"
showNumber (Plural   True)  = "no sing"

showCase :: Case -> String
showCase Genitive   = "Gen."
showCase Accusative = "Akk."
showCase Dative     = "Dat."


-- vi: ft=haskell ts=2 sw=2 et
