{-
 - Language/Ding/Enrich/Grammar/Infer.hs - Enrich grammar annotation within
 -                                         a unit.
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
module Language.Ding.Enrich.Grammar.Infer
  ( expand
  , joinVerbAnnots
  , joinPronounAnnots
  ) where

import Data.List (union)

import Data.NatLang.Grammar


-- | Partition a list according to a function that yields `Maybe' values.
--   Values that are `Nothing' under the function are put in the second
--   partition, for the others, their `Just' value under that function is put
--   into the first partition.
--   Such a function also exists in the `utility-ht' package.
partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe _ []     = ([], [])
partitionMaybe f (x:xs) =
  case f x of
    Just y  -> (y:ys', xs')
    Nothing -> (ys', x:xs')
 where
  (ys', xs') = partitionMaybe f xs


getVTypes :: GrammarInfo -> Maybe [VerbType]
getVTypes (GramLexCategory (PartOfSpeech (Verb vTypes))) = Just vTypes
getVTypes _                                              = Nothing

getPTypes :: GrammarInfo -> Maybe [PronounType]
getPTypes (GramLexCategory (PartOfSpeech (Pronoun pTypes))) = Just pTypes
getPTypes _                                                 = Nothing


-- (TODO): Consider to do a `nub' somewhen.

-- | Merge several verb annotations in a list, if any, to a single one.  The
--   resulting annotation replaces the first of the old annotations
joinVerbAnnots :: [GrammarInfo] -> [GrammarInfo]

-- Note: The location of any first verb annotation is kept.
joinVerbAnnots (GramLexCategory (PartOfSpeech (Verb vTypes)) : as)
  = (GramLexCategory $ PartOfSpeech $ Verb vTypes') : as'
 where
  (vTypess', as') = partitionMaybe getVTypes as
  vTypes' = foldr union [] (vTypes : vTypess')
joinVerbAnnots (a:as) = a : joinVerbAnnots as
joinVerbAnnots []     = []


-- | Merge several pronoun annotations in a list, if any, to a single one.  The
--   resulting annotation replaces the first of the old annotations
joinPronounAnnots :: [GrammarInfo] -> [GrammarInfo]

-- Note: The location of any first pronoun annotation is kept.
joinPronounAnnots (GramLexCategory (PartOfSpeech (Pronoun pTypes)) : as)
  = (GramLexCategory $ PartOfSpeech $ Pronoun pTypes') : as'
 where
  (pTypess', as') = partitionMaybe getPTypes as
  pTypes' = foldr union [] (pTypes : pTypess')
joinPronounAnnots (a:as) = a : joinPronounAnnots as
joinPronounAnnots []     = []


isPlural :: GrammarInfo -> Bool
isPlural (GramLexCategory (Number (Plural _))) = True
isPlural _                                     = False


-- | From a list of (all) grammar annotations on some entity, infer others,
--   which are not explicitly specified in the Ding source.
--
--   For most annotations, this does not infer anything.
--
--   This function should not be applied after any grammar annotation is
--   removed (e.g., when relocating an annotation to a higher level).
--
--   If the argument does not contain duplicates (recommended), neither does
--   the result.
expand :: [GrammarInfo] -> [GrammarInfo]
expand as =
  let as' = concatMap infer as

  -- Notes:
  --  * union first removes duplicates in as', then deletes any element from
  --    it that also is in as (see implementation of union).
  --    This is exactly what we want.
  --  * For longer lists, this would be quite inefficient, but in particular
  --    as' is expected to have few unique elements.
  in  as `union` as'

 where

  infer :: GrammarInfo -> [GrammarInfo]
  infer (GramLexCategory gram) = map GramLexCategory $ inferGLC gram
  infer _                      = []

  -- Infer from single annotations.
  -- Note that {noun} implies {sing} iff {pl} is not annotated.
  inferGLC :: GramLexCategory -> [GramLexCategory]
  inferGLC (Gender _)
    | any isPlural as = [PartOfSpeech Noun]
    | otherwise       = [PartOfSpeech Noun, Number $ Singular False]
  inferGLC (Number (Singular True)) = [PartOfSpeech Noun]
  inferGLC (Number (Plural True))   = [PartOfSpeech Noun]
  inferGLC _                      = []


-- vi: ft=haskell ts=2 sw=2 et
