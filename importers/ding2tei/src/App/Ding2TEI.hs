module App.Ding2TEI where

import Control.Monad (liftM)
import Control.Monad.Trans.State (State, state, evalState)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.NatLang.Dictionary (Dictionary(Dictionary), Body(Body))
import Language.Ding.Syntax (Ding)
import qualified Language.Ding.Syntax as Ding
import Language.TEI.Syntax (TEI)
import qualified Language.TEI.Syntax.Body as TEI
import qualified Language.TEI.Syntax.Reference as TRef


type Id = Int

-- | Map from headwords to the last given id (if any).
type IdMap = Map String Id

type IdState = State IdMap



-- | Modify value and also give out the new value.
--   Similar to `updateLookupWithKey' from `Data.Map.Strict'.
modifyLookup :: Ord k => (Maybe a -> a) -> k -> Map k a -> (a, Map k a)

-- Note that (,) is an instance of Functor, where the first argument remains
-- constant upon `fmap'.  (This is why the following works; v' is passed
-- through.)  See also the documentation of `alterF'.
modifyLookup f = Map.alterF $ \ v -> let v' = f v in (v', Just v')


-- Notes:
--  * TODO: Documentation.
--  * TODO: Consider to subdivide into two steps (labeling with ids,
--          converting)
--    * likely more readable
--    * (slightly?) less efficient
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

    -- TODO: add nEntries to header
    --nEntries = length teiEntries   -- (>= length dingLines)
  in
    if null teiEntries
    then error "No real entry found."
    else Dictionary header srcLang tgtLang (Body teiEntries)


convLines :: [Ding.Line] -> IdState [TEI.Entry]
convLines = liftM concat . mapM convLine


convLine :: Ding.Line -> IdState [TEI.Entry]
convLine (Ding.Line entries) = liftM snd $ convEntries entries []


convEntries :: [Ding.Entry]
            -> [TRef.Ident]
            -> IdState ([TRef.Ident], [TEI.Entry])

convEntries []     _    = return ([], [])
convEntries (e:es) refs = do
  rec
    (refs1, es1) <- convEntry   e  (refs ++ refs2)
    (refs2, es2) <- convEntries es (refs ++ refs1)
  return (refs1 ++ refs2, es1 ++ es2)


-- Naming convention:
--  * gRefs: references to other group's units within the same line
--  * uRefs: references to other units in the same group


convEntry :: Ding.Entry
          -> [TRef.Ident]
          -> IdState ([TRef.Ident], [TEI.Entry])

convEntry (Ding.Entry (Ding.Group keyUnits) valGroup) gRefs =
  convUnits keyUnits []
 where

  translations = groupToTranslations valGroup

  convUnits :: [Ding.Unit]
            -> [TRef.Ident]
            -> IdState ([TRef.Ident], [TEI.Entry])

  convUnits []     _     = return ([], [])
  convUnits (u:us) uRefs = do
    rec
      (uRef',  e ) <- convUnit  u  (uRefs ++ uRefs')
      (uRefs', es) <- convUnits us (uRefs ++ uRef' : [])
    return (uRef' : uRefs', e : es)


  convUnit :: Ding.Unit -> [TRef.Ident] -> IdState (TRef.Ident, TEI.Entry)
  convUnit u uRefs = do
    let headword = Ding.unitHeadword u
    n <- nextId headword
    let ident = TRef.Ident headword n
    return (ident, makeTEIEntry u ident uRefs gRefs translations)


nextId :: String -> IdState Id
nextId str = state $ \ cmap -> modifyLookup (maybe 1 (+1)) str cmap


groupToTranslations :: Ding.Group -> [TEI.Translation]
groupToTranslations (Ding.Group us) = map unitToTranslation us
 where
  unitToTranslation :: Ding.Unit -> TEI.Translation
  unitToTranslation u = TEI.Translation
    { TEI.translationOrth    = Ding.unitHeadword u
    , TEI.translationGrammar = Ding.unitGrammar u
    , TEI.translationUsages  = Ding.unitUsages u
    }


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
        { TEI.senseN            = 1
        , TEI.senseGrammar      = []
        , TEI.senseUsages       = Ding.unitUsages u
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


makeGroupReference :: TRef.Ident -> TRef.Reference
makeGroupReference = makeLinkedReference TRef.Related

makeUnitReference :: TRef.Ident -> TRef.Reference
makeUnitReference = makeLinkedReference TRef.Synonymy

-- make reference with @target.
makeLinkedReference :: TRef.RefType -> TRef.Ident -> TRef.Reference
makeLinkedReference refType ident@(TRef.Ident hw _) =
  TRef.Reference refType (Just ident) hw

-- ~tilde references do not have a @target.
makeTildeReference :: String -> TRef.Reference
makeTildeReference = TRef.Reference TRef.Related Nothing


-- vi: ft=haskell ts=2 sw=2 et
