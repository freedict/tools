{-
 - Language/Ding/Enrich/Grammar.hs - Enrich grammar annotation
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
 - Enrich a line's grammar annotations.
 -
 - This includes inferral and transferral, see
 - `Language.Ding.Enrich.Grammar.Infer' and
 - `Language.Ding.Enrich.Grammar.Transfer', respectively.
 -
 - Note: This module works on lines.  It could equally well work on entries,
 - as `Language.Ding.Enrich.Grammar.Transfer' does.  This is purely done to
 - be in line with other enrichment modules.
 -}
module Language.Ding.Enrich.Grammar (enrichGrammar) where

import Data.NatLang.Grammar (GrammarInfo)
import Language.Ding.Syntax
import Language.Ding.Enrich.Grammar.Infer
  ( expand
  , joinVerbAnnots
  , joinPronounAnnots
  )
import Language.Ding.Enrich.Grammar.Transfer (transfer)


-- Notes:
--  * The order of enrichment steps is deliberately chosen.
--    * Do inferral (expand) first, to allow e.g. `POS Noun' as derived from
--      `SingulareTantum' to be transfered.
--    * Do join*Annots last, to account for any new duplicates.
--  * All functions operate on GramLexCategory, the rest of GrammarInfo
--    (Collocate) is of no importance.
--    * It might be better to separate GramLexCategory and Collocate into
--      two distinct lists in the ASTs (TODO?).
--  ? TODO?: Remove duplicates from the initial list of annotations on a unit?
--  ? TODO?: Testing: Quickcheck.


-- | Enrich a line's grammar annotations.
enrichGrammar :: Line -> Line
enrichGrammar (Line es) = Line $ map enrichEntry es

enrichEntry :: Entry -> Entry
enrichEntry = modEntryGrammar (joinVerbAnnots . joinPronounAnnots)
            . transfer
            . modEntryGrammar expand


-- | Modify the grammar annotations of all units in an entry.
modEntryGrammar :: ([GrammarInfo] -> [GrammarInfo]) -> Entry -> Entry
modEntryGrammar f (Entry g h) =
  Entry (modGroupGrammar f g) (modGroupGrammar f h)

modGroupGrammar :: ([GrammarInfo] -> [GrammarInfo]) -> Group -> Group
modGroupGrammar f (Group us) = Group $ map (modUnitGrammar f) us

modUnitGrammar :: ([GrammarInfo] -> [GrammarInfo]) -> Unit -> Unit
modUnitGrammar f u = u { unitGrammar = f $ unitGrammar u }


-- vi: ft=haskell ts=2 sw=2 et
