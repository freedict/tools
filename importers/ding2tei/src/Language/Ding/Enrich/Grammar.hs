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
 - Enrich a line's grammar annotations and infer such from the presence of
 - inflected forms.
 -
 - This includes inferral from and transferral of grammar annotations, see
 - `Language.Ding.Enrich.Grammar.Infer' and
 - `Language.Ding.Enrich.Grammar.Transfer', respectively,
 - and inferral of grammar information (POS: verb) from the presence of
 - inflected forms, see `Language.Ding.Enrich.Grammar.InflectedForms`.
 -
 - Note: This module works on lines.  It could equally well work on entries,
 - as `Language.Ding.Enrich.Grammar.Transfer' does.  This is purely done to
 - be in line with other enrichment modules.
 -}
module Language.Ding.Enrich.Grammar (enrichGrammar) where

import Data.NatLang.Grammar (GrammarInfo)
import Language.Ding.Syntax
import Language.Ding.Enrich.Grammar.InflectedForms (enrichFromInflectedForms)
import Language.Ding.Enrich.Grammar.Infer
  ( expand
  , joinVerbAnnots
  , joinPronounAnnots
  )
import Language.Ding.Enrich.Grammar.Transfer (transfer)


-- Notes:
--  * The order of enrichment steps is deliberately chosen.
--    * First, identify verbs from the presence of inflected forms; the
--      inflected forms constituting the input of this step do not change
--      during the whole grammar enrichment process.
--    * Do inferral (expand) next and hence before the remainder, to allow
--      e.g. `POS Noun' as derived from `SingulareTantum' to be transfered.
--    * Do join*Annots last, to account for any new duplicates.
--  * All functions operate on GramLexCategory, the rest of GrammarInfo
--    (Collocate) is of no importance.
--    * It might be better to separate GramLexCategory and Collocate into
--      two distinct lists in the ASTs (TODO?).
-- * enrichFromInflectedForms only needs to apply to the english side.
--   * It is, however, applied to both sides -- for reasons of simplicity.
--     * The side's languages are unknown here.
--  ? TODO?: Remove duplicates from the initial list of annotations on a unit?
--  ? TODO?: Testing: Quickcheck.


-- | Enrich a line's grammar annotations.
enrichGrammar :: Line -> Line
enrichGrammar (Line es) = Line $ map enrichEntry es

enrichEntry :: Entry -> Entry
enrichEntry = modEntryGrammar (joinVerbAnnots . joinPronounAnnots)
            . transfer
            . modEntryUnit
                ( modUnitGrammar expand
                . enrichFromInflectedForms
                )


-- | Modify the units in an entry.
modEntryUnit :: (Unit -> Unit) -> Entry -> Entry
modEntryUnit f (Entry g h) =
  Entry (modGroupUnit f g) (modGroupUnit f h)

modGroupUnit :: (Unit -> Unit) -> Group -> Group
modGroupUnit f (Group us) = Group $ map f us

-- | Modify the grammar annotations of all units in an entry.
modEntryGrammar :: ([GrammarInfo] -> [GrammarInfo]) -> Entry -> Entry
modEntryGrammar f e = modEntryUnit (modUnitGrammar f) e

modUnitGrammar :: ([GrammarInfo] -> [GrammarInfo]) -> Unit -> Unit
modUnitGrammar f u = u { unitGrammar = f $ unitGrammar u }


-- vi: ft=haskell ts=2 sw=2 et
