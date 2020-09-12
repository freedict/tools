{-
 - Language/TEI/Show/Grammar.hs - show-like functions for grammar elements
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

module Language.TEI.Show.Grammar
  ( showPrimaryPOS
  , showVerbType
  , showPronounType
  , showGender
  , showNumber
  , showCase
  , shownSingulareTantum
  , shownPluraleTantum
  ) where

import Data.NatLang.Grammar


-- Notes:
--  * The string representations are chosen according to
--    Freedict/fd-dictionaries/shared/FreeDict_ontology.xml
--    * If not found, annotated as UNCERTAIN.
--    * The existing dictionaries may serve as reference also.
--      * swh-eng is the only dictionary to contain <subc> elements.
--      * lat-deu is the only dictionary to contain <case> elements.
--    * Some could not be found anywhere and have been guessed/invented.
--  * This module encompasses show* functions, which are similar to show, as
--    defined in `Show' instances.
--    * They are not defined in `Show' instances, because
--      a) The datatypes are shared between the Ding and TEI ASTs and it is
--         inconvenient to write two complete sets of Show instances by hand.
--      b) Some of the functions are not injective, they hide important
--         information, which is generally undesired for `show'.
--    * These problems do not exist for Usage.  (UsageType has no
--      representation in Ding; all other information is encoded as plain
--      string)


showPrimaryPOS :: PartOfSpeech -> String
showPrimaryPOS Noun         = "n"
showPrimaryPOS (Verb _)     = "v"
showPrimaryPOS Adjective    = "adj"
showPrimaryPOS Adverb       = "adv"
showPrimaryPOS Preposition  = "prep"
showPrimaryPOS Conjunction  = "conj"
showPrimaryPOS Article      = "art"
showPrimaryPOS (Pronoun _)  = "pron"
showPrimaryPOS Numeral      = "num"
showPrimaryPOS Interjection = "int"

showVerbType :: VerbType -> String
showVerbType Transitive   = "trans"
showVerbType Intransitive = "intr"
showVerbType Reflexive    = "refl"        -- UNCERTAIN (~swh-eng)

showPronounType :: PronounType -> String
showPronounType Personal      = "pers"    -- UNCERTAIN (~swh-eng)
showPronounType Interrogative = "inter"   -- UNCERTAIN (~swh-eng)
showPronounType Relative      = "rel"     -- UNCERTAIN

showGender :: Gender -> String
showGender Feminine  = "fem"
showGender Masculine = "masc"
showGender Neuter    = "neut"

showNumber :: Number -> String
showNumber Singular = "sg"                -- UNCERTAIN (~ several)
showNumber Plural   = "pl"                -- UNCERTAIN (~ several)

showCase :: Case -> String
showCase Genitive   = "gen"               -- UNCERTAIN (~lat-deu)
showCase Accusative = "acc"               -- UNCERTAIN (~lat-deu: akk)
showCase Dative     = "dat"               -- UNCERTAIN (~lat-deu)

shownSingulareTantum :: String
shownSingulareTantum = "sg only"          -- UNCERTAIN

shownPluraleTantum :: String
shownPluraleTantum   = "pl only"          -- UNCERTAIN


-- vi: ft=haskell ts=2 sw=2 et
