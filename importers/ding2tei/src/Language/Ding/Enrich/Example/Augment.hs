{-
 - Language/Ding/Enrich/Example/Augment.hs - augment unit with examples
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
 - Inspect a single potential example unit considering a list of known non-
 - examples.
 - If the former is identified as an example for any of the latter, augment the
 - latter with the former and mark the former as example, i.e. to be forgotten.
 -}
module Language.Ding.Enrich.Example.Augment (augmentEntriesWith) where

import Control.Monad (liftM)
import Control.Monad.Trans.Writer (Writer, writer, runWriter)

import Language.Ding.Enrich.Example.Identify (isExampleOf)
import Language.Ding.Syntax (Entry(..), Group(..), Unit(..))
import Language.Common.Syntax (Example(..))


-- | Add the potential example `pex', together with its translations, to
--   any unit on the left side of any entry in `es'.
--   If any change happened, forget about the potential example (first element
--   of result pair is `Nothing'), otherwise `Just' keep it.
augmentEntriesWith :: Unit -> [Unit] -> [Entry] -> (Maybe Unit, [Entry])
augmentEntriesWith pex pexTrans es =
  case runWriter (updateEntries pex pexTrans es) of
    (es', Changed True)  -> (Nothing, es')
    (_,   Changed False) -> (Just pex, es)


newtype Changed = Changed Bool

-- | The (Bool, ||) semigroup.
instance Semigroup Changed where
  (Changed b1) <> (Changed b2) = Changed $ b1 || b2

-- | The (Bool, ||, False) monoid.
instance Monoid Changed where
  mempty = Changed False

-- | A writer monad used to remember whether any change was done.
--   Uses the natural Bool monoid with (||).
type ChangeWriter = Writer Changed

-- | Return a value in the `ChangeWriter' monad and indicate a change.
returnChanged :: a -> ChangeWriter a
returnChanged a = writer (a, Changed True)

-- | Return a value in the `ChangeWriter' monad and indicate that nothing
--   changed.
--   Equivalent to `return'.
returnUnchanged :: a -> ChangeWriter a
returnUnchanged a = writer (a, Changed False)


-- | Augment entries with the provided example, wherever it matches.
--   If any change happens, this is recorded in the `ChangeWriter' result.
updateEntries :: Unit -> [Unit] -> [Entry] -> ChangeWriter [Entry]
updateEntries _   _        []     = returnUnchanged []
updateEntries pex pexTrans (e:es) =
  (:) <$> updateEntry pex pexTrans e <*> updateEntries pex pexTrans es

updateEntry :: Unit -> [Unit] -> Entry -> ChangeWriter Entry
updateEntry pex pexTrans (Entry g h) = do
  g' <- updateGroup pex pexTrans g
  return $ Entry g' h

updateGroup :: Unit -> [Unit] -> Group -> ChangeWriter Group
updateGroup pex pexTrans (Group us) = liftM Group $ updateUnits pex pexTrans us

updateUnits :: Unit -> [Unit] -> [Unit] -> ChangeWriter [Unit]
updateUnits _   _        []     = returnUnchanged []
updateUnits pex pexTrans (u:us) =
  (:) <$> (updateUnit pex pexTrans u) <*> (updateUnits pex pexTrans us)

-- Note that examples are appended, using (++), to keep the order of the
-- examples.  This inefficient for large lists.  The lists are expected to be
-- very small though, often ending up with only a single element, if any.
--  - Runtime with example-identification actually seems to have (slightly)
--    dropped (likely due to smaller number of entries generated).
--    - Nevertheless, the (++) is hence ok.
updateUnit :: Unit -> [Unit] -> Unit -> ChangeWriter Unit
updateUnit pex pexTrans key = do
  if (unitHeadword pex) `isExampleOf` (unitHeadword key)
  then
    returnChanged $ key { unitExamples = unitExamples key ++ ex : [] }
  else
    returnUnchanged key
 where
  ex = Example (unitPlain pex) (map unitPlain pexTrans)


-- vi: ft=haskell ts=2 sw=2 et
