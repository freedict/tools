{-
 - Language/Ding/Enrich/Grammar/Transfer.hs - Transfer grammar annotation
 -                                            within an entry.
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
 - Transfer grammar annotation between units in an entry.
 - Units of the same and of the respective other language are treated the same.
 - As of now, only part of speech annotation is considered transferable,
 - further excluding verb types (transitivity, reflexivity).
 -}
module Language.Ding.Enrich.Grammar.Transfer (transfer) where


import Data.Maybe (mapMaybe)
import Data.List (union)

import Language.Ding.Syntax (Entry(..), Group(..), Unit, unitGrammar)
import Data.NatLang.Grammar


-- Note
--  * The things in here could also be done in an attribute grammar (~Happy).


-- | Transfer grammar annotations within an entry.
transfer :: Entry -> Entry
transfer (Entry g h) = Entry g' h'
 where
  -- Notice how the below only works because of lazyness (and Haskell's
  -- automated fixed point calculation).
  -- (The below two function evaluations depend on results of one another.)
  (as1, g') = handleGroup as2 g
  (as2, h') = handleGroup as1 h


handleGroup :: [GrammarInfo] -> Group -> ([GrammarInfo], Group)
handleGroup as (Group us) = fmap Group $ handleUnits as us

handleUnits :: [GrammarInfo] -> [Unit] -> ([GrammarInfo], [Unit])
handleUnits _  []     = ([], [])

-- Note:
--  * (++) may cause many duplicates here.  Since the lists are short, it
--    is however ok (possibly better) to remove the duplicates once in the end
--    (at the leaves/units -> repeatedly).
handleUnits as (u:us) = (as1 ++ as2, u':us')
 where
  -- Again, this works because of Haskell's lazyness.
  (as1, u')  = handleUnit  (as ++ as2) u
  (as2, us') = handleUnits (as ++ as1) us

handleUnit :: [GrammarInfo] -> Unit -> ([GrammarInfo], Unit)
handleUnit as u = (transferableGrammar uas, u { unitGrammar = uas `union` as })
 where
  uas = unitGrammar u


-- | From grammar information, extract any that is considered transferable to
--   units in the same entry (i.e., the same group and the translation group).
--
--   Duplicates may arise.  These are not removed, because the result of this
--   application is to be merged with `union' anyways (union removes duplicates
--   off its second argument).
transferableGrammar :: [GrammarInfo] -> [GrammarInfo]
transferableGrammar = mapMaybe reduceGram


-- Note: Collocations such as {+ Gen} are dropped.  In many cases, co-occuring
--       units share such collocations (without explicit annotation on each),
--       but these cases are hard to identify.
reduceGram :: GrammarInfo -> Maybe GrammarInfo
reduceGram (GramLexCategory gram) = fmap GramLexCategory $ reduceGLC gram
reduceGram _                      = Nothing


-- | Extract grammar information that shall be transfered to other units
--   in the same entry (i.e., the same group and the translation group).
--
--   Note: Handling in-language synonyms and translations the same way is not
--         natural, but works for a German-English dictionary with the present
--         grammar annotations.  The grammatical case might form an exception,
--         but it is rarely annotated anyways (possibly even no occurence).
reduceGLC :: GramLexCategory -> Maybe GramLexCategory
reduceGLC (PartOfSpeech pos) = fmap PartOfSpeech $ reducePOS pos
reduceGLC _                  = Nothing


-- | Retain all information except verb types (reflexivity & transitivity)
--
--   Notes:
--    * Proposition-annotations ({prp}) are often made seemingly incorrect.
--    * It is assumed preposition means adposition.
--      * See https://en.wikipedia.org/wiki/Preposition_and_postposition
--      * Otherwise, a preposition might be translated to a postposition.
reducePOS :: PartOfSpeech -> Maybe PartOfSpeech
reducePOS (Verb _)  = Just $ Verb []
reducePOS pos       = Just pos


-- vi: ft=haskell ts=2 sw=2 et
