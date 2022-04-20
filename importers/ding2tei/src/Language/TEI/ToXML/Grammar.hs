{-
 - Language/TEI/ToXML/Grammar.hs - convert grammar information to TEI XML (AST)
 -
 - Copyright 2020,2022 Einhard Leichtfuß
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
 - Convert grammar annotations to TEI XML.
 -}
module Language.TEI.ToXML.Grammar (convGrammar) where


import Text.XML.Light

import Data.NatLang.Grammar
import Data.NatLang.Usage (Usage)
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
convGrammarInfo (Collocate colloc usgs) = pure $ convCollocate colloc usgs


-- Note: It seems impossible to represent the present usage annotations
--       (TODO).
convCollocate :: Collocate -> [Usage] -> Element
convCollocate (CollocCase iProns cas) _usgs = unode "colloc" $
  "[" ++ prefix ++ "+ " ++ showCase cas ++ "]"
 where
  prefix =
    case iProns of
      []     -> ""
      (p:ps) -> p ++ concatMap (", " ++) ps

convCollocate (CollocPOS pos)         _usgs = unode "colloc" $
  "[+ " ++ showPrimaryPOS pos ++ subType ++ "]"
 where
  subType =
    case pos of
      (Verb vTypes)    -> concatMap ((' ':) . showVerbType)    vTypes
      (Pronoun pTypes) -> concatMap ((' ':) . showPronounType) pTypes
      _                   -> ""

-- Note: Ignore potential singulare/plurale tantum attribute.
--   - Does not occur in Ding version 1.9.
convCollocate (CollocNumber number)   _usgs = unode "colloc" $
  "[+ " ++ showPrimaryNumber number ++ "]"


convGramLexCat :: GramLexCategory -> [Element]
convGramLexCat (PartOfSpeech pos) = convPOS pos
convGramLexCat (Gender gen)       = [unode "gen" (showGender gen)]
convGramLexCat (Number num)       = unode "number" (showPrimaryNumber num)
                                  : convNumberSubc num
convGramLexCat (Case cas)         = [unode "case" (showCase cas)]


convPOS :: PartOfSpeech -> [Element]
convPOS pos = unode "pos" (showPrimaryPOS pos)
  : case pos of
      (Verb vTypes)    -> map (unode "subc" . showVerbType) vTypes
      (Pronoun pTypes) -> map (unode "subc" . showPronounType) pTypes
      _                   -> []


convNumberSubc :: Number -> [Element]
convNumberSubc (Singular True) = [unode "subc" shownSingulareTantum]
convNumberSubc (Plural   True) = [unode "subc" shownPluraleTantum]
convNumberSubc _               = []


-- References:
--  * [0] https://dariah-eric.github.io/lexicalresources/pages/TEILex0/TEILex0.html#collocates

-- vi: ft=haskell ts=2 sw=2 et
