{-
 - Language/Ding/Enrich/Grammar/FromInflectedForms.hs - Recognize that inflec-
 -                                                      ted forms are only
 -                                                      annotated to verbs.
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
 - From a set of grammar annotations on a unit, infer others to apply to the
 - same unit.
 -
 - Note that this works on grammar elements which are common to the Ding and
 - TEI, however this module is deliberately placed under `Language.Ding',
 - because the enrichment depends on inference rules which are particular to
 - the Ding.  E.g. `{f}' implies that the corresponding unit is a noun, in the
 - Ding.  There may however, in general, very well be other parts of speach
 - bearing a gender.
 -}
module Language.Ding.Enrich.Grammar.InflectedForms
  ( enrichFromInflectedForms
  ) where

import Data.NatLang.Grammar
  ( GrammarInfo(GramLexCategory)
  , GramLexCategory(PartOfSpeech)
  , PartOfSpeech(Verb)
  )
import Language.Ding.Syntax (Unit(unitInflected, unitGrammar))


-- | Annotate any unit that is annotated with inflected forms as a verb.
--   Such new verb annotations are appended to the existing grammar
--   annotations.
enrichFromInflectedForms :: Unit -> Unit
enrichFromInflectedForms u =
  case unitInflected u of
    (Just _) -> u { unitGrammar = unitGrammar u ++ verbAnnot : [] }
    Nothing  -> u
 where
  verbAnnot = GramLexCategory $ PartOfSpeech $ Verb []


-- vi: ft=haskell ts=2 sw=2 et
