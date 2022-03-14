{-
 - Language/Ding/Pretty.hs - pretty printing of the Ding AST
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

{-# LANGUAGE FlexibleInstances #-}

{-|
 - Pretty printing of the Ding AST.
 -
 - Note that the pretty printing does not generally reproduce the originally
 - parsed Ding syntax; the result may even not be accepted by the parser.
 -
 - The former is because
 -  a) during parsing, some information is dropped,
 -  b) enrichment may have changed the order of annotations.
 - Also, potentially identified examples are not displayed.
 -
 - The latter is only the case when grammar inferral (enrichment) was
 - performed.  In this case, grammar information such as "{noun}" may be
 - inferred, which is not originally present in the Ding dictionary, albeit
 - useful information during debugging.
 -}
module Language.Ding.Pretty (pretty) where


import Prelude hiding ((<>))

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import Text.PrettyPrint

import Data.NatLang.Dictionary (Dictionary(..), Body(..))
import Data.NatLang.Grammar
import Data.NatLang.InflectedForms
import Data.NatLang.Usage
import Language.Ding.Show.Grammar (showGLC, showCase, showNumber, showPOS)
import Language.Ding.Syntax hiding (Dictionary)


-------------------------
-- Some helper functions

-- | Modify a `Doc' iff it is non-empty.
applyNE :: (Doc -> Doc) -> Doc -> Doc
applyNE f x = if isEmpty x then empty else f x

-- | Like `punctuate', but remove nonempty `Doc's first.
punctuateNE :: Doc -> [Doc] -> [Doc]
punctuateNE p = punctuate p . filter (not . isEmpty)

slashes :: Doc -> Doc
slashes x = char '/' <> x <> char '/'


class Pretty a where
  pretty :: a -> Doc

  prettyList :: [a] -> Doc
  prettyList = error
    "Language.Ding.Pretty: This element does not occur in lists."

  prettyNEList :: NonEmpty a -> Doc
  prettyNEList = prettyList . NEList.toList



------------------------
-- The Pretty instances


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


instance Pretty Entry where
  pretty (Entry g h) = pretty g <+> text "::" <+> pretty h


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


instance Pretty GrammarInfo where
  pretty (GramLexCategory gram)  = pretty gram
  pretty (Collocate colloc usgs) = pretty colloc <+> prettyList usgs

  prettyList = applyNE braces . hsep . punctuate semi . map pretty

instance Pretty GramLexCategory where
  pretty = hcat . punctuate (char '/') . map text . showGLC

instance Pretty Collocate where
  pretty (CollocCase iProns cas) = pref <+> char '+' <> pretty cas
   where
    pref = hsep $ punctuate comma $ map text iProns
  pretty (CollocPOS pos)         = char '+' <> pretty pos
  pretty (CollocNumber number)   = char '+' <> pretty number

instance Pretty Case where
  pretty = text . showCase

instance Pretty Number where
  pretty = text . showNumber

instance Pretty PartOfSpeech where
  -- Note: showPOS is expected to always give exactly one string here.
  pretty = hcat . punctuate (char '/') . map text . showPOS

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
