{-
 - App/Ding2TEI.hs - translate the Ding to TEI (AST)
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
 - Translate the `Ding' to `TEI'.
 -}
module App.Ding2TEI where

import Control.Monad (liftM)
import Control.Monad.Trans.State (State, state, evalState)

-- When laziness is not required, Data.Map.Strict is to be preferred over
-- the default Data.Map.Lazy.
--  - See: <https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Strict.html>
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.NatLang.Dictionary (Dictionary(Dictionary), Body(Body))
import Data.NatLang.Usage (Usage( Usage ))
import qualified Data.NatLang.Usage as Usage
import Language.Ding.Syntax (Ding)
import qualified Language.Ding.Syntax as Ding
import Language.TEI.Syntax (TEI)
import qualified Language.TEI.Syntax as TEI
import qualified Language.TEI.Syntax.Reference as TRef


-- | Map from headwords to the last number given to that headword (if any).
type IdMap = Map String Int

type IdState = State IdMap



-- | Modify value and also give out the new value.
--   Similar to `updateLookupWithKey' from `Data.Map.Strict'.
modifyLookup :: Ord k => (Maybe a -> a) -> k -> Map k a -> (a, Map k a)

-- Note that (,) is an instance of Functor, where the first argument remains
-- constant upon `fmap'.  (This is why the following works; v' is passed
-- through.)  See also the documentation of `alterF'.
modifyLookup f = Map.alterF $ \ v -> let v' = f v in (v', Just v')


-- Notes:
--  * Computation is wrapped in the IdState monad.
--    * This is to allow for the distribution of unique identifiers, which
--      should be of the form `HW.n'. where `HW' is the headword and `n' is
--      a number unique for that headword--naturally chosen as the entry's
--      rank in the left-to-right order of headwords of the same name.
--    * The IdState monad in particular is a MonadFix, which does allow to
--      not only distribute identifiers using a state, but also to access
--      identifiers of "later" to be constructed entries.
--      * In short, MonadFix allows for bidirectional flow of information,
--        albeit the state is only transferred left-to-right.
--      * We use this to provide entries that stem from the same line with
--        references to one another--also to later ones.
--        * Alternatively, one could have separated this in two steps
--          (labeling, translating), requiring an intermediate datatype (or
--          some Maybe values) and a whole new set of (trivial) AST traversing
--          functions.
--    * See also: doc/thesis.pdf
--  * There may be several orthographically identical headwords in a line.
--  * Concatenating reference lists all around (often the shorter to the longer
--    list) is not very efficient.  To keep the order of the references,
--    something like this is required though.
--    * Alternative: Use a queue or a difference list.
--    * Note that the lists are usually quite small.


-- | Convert a Ding AST to TEI AST.
--   Errors when the result contains no entries.  (The FreeDict XML schema
--   requires at least one entry.)
--   Note that the existance of a line in the Ding does not imply the
--   existance of an entry in the corresponding TEI.
--    - The Ding is permitted to contain empty lines - " :: " .
ding2tei :: Ding -> TEI
ding2tei (Dictionary header srcLang tgtLang body) =
  let
    (Body dingLines) = body
    teiEntries = evalState (convLines dingLines) Map.empty
  in
    if null teiEntries
    then error "No real entry found."
    else Dictionary header srcLang tgtLang (Body teiEntries)


-- | Translate a list of lines, annotating the resulting TEI entries with
--   unique identifiers.
convLines :: [Ding.Line] -> IdState [TEI.Entry]
convLines = liftM concat . mapM convLine


convLine :: Ding.Line -> IdState [TEI.Entry]
convLine (Ding.Line entries) = liftM snd $ convEntries entries []


-- | Translate a list of entries.
--   Takes as argument--next to the list of entries to translate--the list
--   of TEI entries' identifiers that result from earlier (more left) Ding
--   entries in the same line.
--   Wrapped om the IdState monad, returns both the resulting list of TEI
--   entries and the identifiers of these.
convEntries :: [Ding.Entry]
            -> [TRef.Ident]
            -> IdState ([TRef.Ident], [TEI.Entry])

-- Notes:
--  * We want to provide `convEntry' with references (identifiers) from both
--    left and right of it.
--    * Typically, monadic computations would only allow to pass such
--      information from left to right.
--      * Monads being instances of MonadFix, however, effectively are stripped
--        of this restriction.
--        * MonadFix has mfix, which can--analogously to fix for
--          cyclic let-expressions--be used to have "information flow
--          backwards"; particularly there may also be apparent cyclic
--          statements.
--    * We obtain references from the left, which we pass both to
--      convEntry and the recursive convEntries call.
--      * convEntry additionally obtains the references from the right,
--        as produced by the recursive convEntries call.
--      * The recursive convEntries call additionally gets the reference
--        resulting from the convEntry call--as it also stems from left of
--        the recursively to be processed entries.
--      * Note that the order of reference concatenation somewhat matters;
--        we'd like to keep them in order.
--  * We could have only passed around TEI entries, these contain the
--    identifiers.
convEntries []       _    = return ([], [])
convEntries (de:des) refs = do
  rec
    (refs1, tes1) <- convEntry   de  (refs ++ refs2)
    (refs2, tes2) <- convEntries des (refs ++ refs1)
  return (refs1 ++ refs2, tes1 ++ tes2)


-- Naming convention:
--  * gRefs: references to other group's units within the same line
--  * uRefs: references to other units in the same group


-- | Translate a single Ding entry to a list of TEI entries.  (Ding entries
--   with several keywords (units in the source group) are split up into
--   equally many TEI entries, linking to one another as synonyms.)
--
--   Takes as additional argument the identifiers to all TEI entries that
--   result from the same line, albeit different Ding entries.  These are
--   used to create "related"-references.
convEntry :: Ding.Entry
          -> [TRef.Ident]
          -> IdState ([TRef.Ident], [TEI.Entry])

convEntry (Ding.Entry (Ding.Group keyUnits) valGroup) gRefs =
  convUnits keyUnits []
 where

  -- Note:
  --  * Functions are local (in a where block) here, to not need to pass around
  --    the translations and `gRefs', which are only needed in the
  --    leaf-function, convUnit.

  translations = groupToTranslations valGroup

  -- | Create a list of TEI entries from a list of Ding units belonging to the
  --   same group (all sharing a common translation group).
  --
  --   Resulting TEi entries are linked to one another as synonyms.
  convUnits :: [Ding.Unit]
            -> [TRef.Ident]
            -> IdState ([TRef.Ident], [TEI.Entry])

  -- Note: Analogous to convEntries.
  convUnits []     _     = return ([], [])
  convUnits (u:us) uRefs = do
    rec
      (uRef',  e ) <- convUnit  u  (uRefs ++ uRefs')
      (uRefs', es) <- convUnits us (uRefs ++ uRef' : [])
    return (uRef' : uRefs', e : es)


  -- | Create a single TEI entry from a Ding unit and the corresponding
  --   `translations', annotating it with references to synonymous and related
  --   entries (the local argument referring to synonymous entries, stemming
  --   from the same Ding entry/group).
  convUnit :: Ding.Unit -> [TRef.Ident] -> IdState (TRef.Ident, TEI.Entry)
  convUnit u uRefs = do
    let headword = Ding.unitHeadword u
    n <- nextId headword
    let ident = TRef.Ident headword n
    return (ident, makeTEIEntry u ident uRefs gRefs translations)


-- | To a string (headword), get the next id (counter) and update the state
--   accodingly.
nextId :: String -> IdState Int

-- Note:
--  * Non-existing entries in the map are considered of value 0.
--    * The first provided number is 1.
--      * Dictionary people seem to like 1-based counting.
nextId str = state $ \ cmap -> modifyLookup (maybe 1 (+1)) str cmap


-- | Transform a `Group' to a list of corresponding TEI translation elements.
groupToTranslations :: Ding.Group -> [TEI.Translation]
groupToTranslations (Ding.Group us) = map unitToTranslation us
 where
  unitToTranslation :: Ding.Unit -> TEI.Translation
  unitToTranslation u = TEI.Translation
    { TEI.translationOrth      = Ding.unitHeadword u
    , TEI.translationGrammar   = Ding.unitGrammar u

    -- Note:
    --  * Parenthesis-enclosed prefixes are translated to
    --    `usg[@type="colloc"]'.
    , TEI.translationUsages    = Ding.unitUsages u
                               ++ map prefixAnnotToUsage (Ding.unitPrefixes u)
    , TEI.translationAbbrevs   = Ding.unitAbbrevs u
    , TEI.translationInflected = Ding.unitInflected u
    , TEI.translationNotes     = Ding.unitSuffixes u
    }


-- | Create a TEI entry.
makeTEIEntry :: Ding.Unit
             -> TRef.Ident
             -> [TRef.Ident]
             -> [TRef.Ident]
             -> [TEI.Translation]
             -> TEI.Entry

makeTEIEntry u ident uRefs gRefs translations = TEI.Entry
  { TEI.entryIdent = ident

  , TEI.entryForm = TEI.Form
      { TEI.formOrth      = Ding.unitHeadword u
      , TEI.formAbbrevs   = Ding.unitAbbrevs u
      , TEI.formInflected = Ding.unitInflected u
      }

  , TEI.entryGrammar = Ding.unitGrammar u

  , TEI.entrySenses = [
      TEI.Sense
        { TEI.senseGrammar      = []
        , TEI.senseUsages       = Ding.unitUsages u
                                ++ map prefixAnnotToUsage (Ding.unitPrefixes u)
        , TEI.senseTranslations = translations
        , TEI.senseExamples     = Ding.unitExamples u
        , TEI.senseReferences   =
               map makeUnitReference  uRefs
            ++ map makeGroupReference gRefs
            ++ map makeTildeReference (Ding.unitReferences u)
        , TEI.senseNotes        = Ding.unitSuffixes u
        }
    ]
  }


-- | Create a "related" reference from an identifier.
makeGroupReference :: TRef.Ident -> TRef.Reference
makeGroupReference = makeLinkedReference TRef.Related

-- | Create a "synonymy" reference from an identifier.
makeUnitReference :: TRef.Ident -> TRef.Reference
makeUnitReference = makeLinkedReference TRef.Synonymy

-- | Create a reference with `\@target'.
makeLinkedReference :: TRef.RefType -> TRef.Ident -> TRef.Reference
makeLinkedReference refType ident@(TRef.Ident hw _) =
  TRef.Reference refType (Just ident) hw

-- | Create a reference without `\@target', as is required for \~tilde
--   references.  (In fact, one could infer such a target, whenever there is
--   exactly one TEI entry with the headword referred to.)
makeTildeReference :: String -> TRef.Reference
makeTildeReference = TRef.Reference TRef.Related Nothing

-- | In TEI, optional prefixes can be encoded as collocates.
--
--   Note:
--    * Depending on the content (e.g., "etw."), <colloc> might be more
--      appropriate, but such content analysis is not done.
prefixAnnotToUsage :: String -> Usage
prefixAnnotToUsage pref = Usage Usage.Colloc pref


-- vi: ft=haskell ts=2 sw=2 et
