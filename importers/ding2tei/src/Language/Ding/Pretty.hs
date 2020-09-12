{-
 - Language/Ding/Pretty.hs - pretty printing of the Ding AST
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

{-# LANGUAGE FlexibleInstances #-}

module Language.Ding.Pretty
  ( pretty
  ) where

import Prelude hiding ((<>))

import Data.NatLang.Dictionary (Dictionary(..), Body(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map.Strict as Map
import Text.PrettyPrint

import Data.NatLang.Grammar
import Data.NatLang.GrammarInfo
import Data.NatLang.InflectedForms
import Data.NatLang.Usage
import Language.Ding.Syntax hiding (Dictionary)
import Language.Ding.Syntax.Grammar (grammarMapRev, caseMapRev, posMapRev)


-- | Modify a `Doc' iff it is non-empty.
applyNE :: (Doc -> Doc) -> Doc -> Doc
applyNE f x = if isEmpty x then empty else f x

slashes :: Doc -> Doc
slashes x = char '/' <> x <> char '/'


class Pretty a where
  pretty :: a -> Doc

  prettyList :: [a] -> Doc
  prettyList = error
    "Language.Ding.Pretty: This element does not occur in lists."

  prettyNEList :: NonEmpty a -> Doc
  prettyNEList = prettyList . NEList.toList


instance Pretty Ding where
  pretty (Dictionary header _ _ body) = pretty header $+$ pretty body


instance Pretty (Body Line) where
  pretty (Body lines) = prettyList lines


-- See also: Language.Ding.HeaderParser
instance Pretty Header where
  pretty (Header version date copyrightHolder copyrightPeriod license url) =
    foldr ($+$) empty $
      [ text "# Version"       <+> text "::" <+> text version <+> text date
      , text "# Copyright (c)" <+> text "::" <+> text copyrightHolder
          <> char ','
      , text "#" <+> text copyrightPeriod
      , text "# License"       <+> text "::" <+> text license
      , text "# URL"           <+> text "::" <+> text url
      ]


instance Pretty Line where

  pretty (Line entries) = prettyList left <+> text "::" <+> prettyList right
   where
    unzipEntries :: [Entry] -> ([Group], [Group])
    unzipEntries = unzip . map (\ (Entry l r ) -> (l, r))

    (left, right) = unzipEntries entries

  prettyList = foldr ($+$) empty . map pretty


instance Pretty Group where
  pretty (Group us) = prettyList us

  prettyList = hcat . punctuate (text " | ") . map pretty


instance Pretty Unit where
  pretty u = hsep $
    [ hsep $ map (parens . text) $ unitPrefixes u
    , text $ unitHeadword u
    , prettyList $ unitGrammar u
    , prettyList $ unitUsages u
    , applyNE slashes $ hsep $ punctuate semi $ map text $ unitAbbrevs u
    , maybe empty pretty $ unitInflected u
    , hsep $ map (parens . text) $ unitSuffixes u
    , hsep $ map (text . ('~':)) $ unitReferences u
    ]

  prettyList = hsep . punctuate semi . map pretty


instance Pretty GramLexCategory where
  pretty gram = case Map.lookup gram grammarMapRev of
    Nothing -> error "Language.Ding.Pretty: Unknown GramLexCategory."
    Just v  -> text v

instance Pretty GrammarInfo where
  pretty (GramLexCategory gram)  = pretty gram
  pretty (CollocCase iProns cas) = pref <+> char '+' <> pretty cas
   where
    pref = hsep $ punctuate comma $ map text iProns
  pretty (CollocPOS pos)         = char '+' <> pretty pos

  prettyList = applyNE braces . hsep . punctuate semi . map pretty

instance Pretty Case where
  pretty cas = case Map.lookup cas caseMapRev of
    Nothing -> error "Language.Ding.Pretty: Unknown grammatical case."
    Just v  -> text v

instance Pretty PartOfSpeech where
  pretty pos = case Map.lookup pos posMapRev of
    Nothing -> error "Language.Ding.Pretty: Unknown part of speech."
    Just v  -> text v

instance Pretty Usage where
  pretty (Usage _ str) = text str
  prettyList = applyNE brackets . hsep . punctuate comma . map pretty

instance Pretty InflectedForms where
  pretty (InflectedForms infl1 infl2) =
    braces $ prettyNEList infl1 <> semi <+> prettyNEList infl2

instance Pretty InflectedForm where
  pretty (InflectedForm form usgs) = text form <+> prettyList usgs
  prettyList = hsep . punctuate comma . map pretty


-- vi: ts=2 sw=2 et
