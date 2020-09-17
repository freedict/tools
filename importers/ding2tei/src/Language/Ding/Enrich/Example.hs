{-
 - Language/Ding/Enrich/Example.hs - example identification and linking
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
 - Identify examples in a line, remove them from it and annotate to the units
 - they belong to.
 -
 - Examples are identified on the unit-level.  That is, a group may contain
 - both examples and non-examples.  If it contains only non-examples, the
 - whole entry (!) is taken off the line.
 - As a side effect, an entry that did not contain any unit on the source side
 - from the beginning, is also removed.
 -
 - Examples are only recognized when occuring right of the unit they exemplify.
 -
 - Note that the enrichment done by `inferExamples' only happens on the source
 - (left) side of the dictionary, therefore the dictionary becomes directional.
 - This is because example identification may have different consequences
 - on both sides, in particular entries may be lost.
 -
 - If both directed outputs (deu-eng, eng-deu) are desired, this module's
 - enrichment should happen separately (and therefore twice).
 -
 - Examples are built from a unit in a group on the left side and the set
 - of corresponding translations, which are in the surrounding entry.
 -
 - Note that this enrichment should not be done on the TEI AST -- as might seem
 - more natural -- because
 -  a) this causes unnecessary calculations on later identified examples,
 -     in particular
 -  b) there would be IDs generated for examples, potentially causing
 -     unnecesarily high number suffixes in other (retained) entries' IDs
 -     (likely rare and not really important).
 -}
module Language.Ding.Enrich.Example (inferExamples) where

import Control.Monad (when, liftM)
import Control.Monad.Trans.State (State, state, modify, get, evalState)

import Language.Ding.Enrich.Example.Augment (augmentEntriesWith)
import Language.Ding.Enrich.Example.Identify (isPotentialExample)
import Language.Ding.Syntax (Line(..), Entry(..), Group(..), Unit(..))

-- Notes:
--  * Only non-examples need to be used to search for examples.
--  * Examples are right of their exemplified units, so go from left to right.
--  * A unit is a known non-example iff it is not an example to all preceding
--    (non-example) units.
--  * Therefore, stepwise (left to right) consider units for being examples.
--    * Units in the same group may be considered in any order, they should
--      not be examples of one another.
--    * In any step test all previously identified non-examples for the new
--      unit exemplifying them.
--  * Two things need to happen.
--    * a) remove units (and possibly entries) when examples.
--    * b) add examples to the units they exemplify.
--    * This is achieved by accumulating all identified non-examples in a
--      state monad.
--      * The state's preceding non-example list is used for any new potential
--        example.
--      * Any newly identified non-example is pushed to the state.
--      * The entries in the state are reversed, since the state is used as a
--        stack.
--        * This is not a problem, their order does not matter when
--          identifying examples.
--        * The list needs to be reversed in the end though.


-- Note: State [Group] (or even State [Unit]) would suffice, at the expense of
--       readability / complexity of the code.  Might be more efficient though.

-- | A stack state to accumulate the entries.
type EntryStackState = State [Entry]

-- | Push an element to a state's stack.
push :: s -> State [s] ()
push x = modify (x:)


-- | In a line, link any examples to the units (on the source side of the
--   dictionary) they are examples to and remove any found examples from
--   the line.
inferExamples :: Line -> Line

-- The entries are accumulated in reverse in the state monad, hence the need
-- for a reversal.
inferExamples (Line entries) =
  Line $ reverse $ evalState (handleEntries entries >> get) []


-- | Perform the enrichment on a list of entries.
--   The result is pushed to the state's stack.
handleEntries :: [Entry] -> EntryStackState ()
handleEntries []     = return ()
handleEntries (e:es) = do
  handleEntry   e
  handleEntries es

handleEntry :: Entry -> EntryStackState ()
handleEntry (Entry g gTrans) = do
  g'@(Group us') <- handleGroup g gTrans
  when (not $ null us') $ push $ Entry g' gTrans

handleGroup :: Group -> Group -> EntryStackState Group
handleGroup (Group us) (Group uTrans) = liftM Group $ handleUnits us uTrans

handleUnits :: [Unit] -> [Unit] -> EntryStackState [Unit]
handleUnits []     _      = return []
handleUnits (u:us) uTrans = do
  mu' <- handleUnit  u  uTrans
  us' <- handleUnits us uTrans
  return $ maybe id (:) mu' us'

handleUnit :: Unit -> [Unit] -> EntryStackState (Maybe Unit)
handleUnit u uTrans =
  if isPotentialExample (unitHeadword u)
  then state $ augmentEntriesWith u uTrans
  else return $ Just u


-- vi: ft=haskell ts=2 sw=2 et
