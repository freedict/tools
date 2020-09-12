{-
 - Language/TEI/ToXML/Grammar.hs - convert grammar information to TEI XML (AST)
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

module Language.TEI.ToXML.Grammar (convGrammar) where


import Text.XML.Light

import Data.NatLang.Grammar
import Data.NatLang.GrammarInfo
import Language.TEI.Show.Grammar


-- | Convert a list of grammar informations to a gramGrp element.
--   If the list is empty, evaluates to `Nothing'.
convGrammar :: [GrammarInfo] -> Maybe Element
convGrammar [] = Nothing
convGrammar gs = Just $ unode "gramGrp" $ concatMap convGrammarInfo gs


-- Notes:
--  * For collocates, follows the suggestion from TEI Lex-0 (however stick with
--    <colloc>) [0].
--  * For POS collocates, a potential subtype is simply appended.
--  * For Case collocates, the potential interrogative pronouns are prepended,
--    in the same form they appear in the Ding.
convGrammarInfo :: GrammarInfo -> [Element]
convGrammarInfo (GramLexCategory gram)  = convGramLexCat gram

convGrammarInfo (CollocCase iProns cas) = pure $ unode "colloc" $
  "[" ++ prefix ++ "+ " ++ showCase cas ++ "]"
 where
  prefix =
    case iProns of
      []     -> ""
      (p:ps) -> p ++ concatMap (", " ++) ps

convGrammarInfo (CollocPOS pos)         = pure $ unode "colloc" $
  "[+ " ++ showPrimaryPOS pos ++ subType ++ "]"
 where
  subType =
    case pos of
      (Verb vTypes)    -> concatMap ((' ':) . showVerbType)    vTypes
      (Pronoun pTypes) -> concatMap ((' ':) . showPronounType) pTypes
      _                   -> ""


convGramLexCat :: GramLexCategory -> [Element]
convGramLexCat (PartOfSpeech pos) = convPOS pos
convGramLexCat (Gender gen)       = [unode "gen" (showGender gen)]
convGramLexCat (Number num)       = [unode "number" (showNumber num)]
convGramLexCat (SingulareTantum)  = [unode "number" shownSingulareTantum]
convGramLexCat (PluraleTantum)    = [unode "number" shownPluraleTantum]
convGramLexCat (Case cas)         = [unode "case" (showCase cas)]

convPOS :: PartOfSpeech -> [Element]
convPOS pos = unode "pos" (showPrimaryPOS pos)
  : case pos of
      (Verb vTypes)    -> map (unode "subc" . showVerbType) vTypes
      (Pronoun pTypes) -> map (unode "subc" . showPronounType) pTypes
      _                   -> []


-- References:
--  * [0] https://dariah-eric.github.io/lexicalresources/pages/TEILex0/TEILex0.html#collocates

-- vi: ft=haskell ts=2 sw=2 et
