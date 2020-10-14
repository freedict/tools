{-
 - Language/TEI/Syntax.hs - general AST structures
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
 - The TEI AST.
 -
 - The types are quite closely related to TEI's XML elements (see
 - `Language.TEI.ToXML' on how they translate.
 -
 - This does only support a small subset of TEI (for dictionaries), as is
 - needed to represent the information that stems from the Ding dictionary.
 -
 - AST elements that are common to both Ding and TEI are to be found in
 - `Data.NatLang'.
 -
 - The TEI header is not explicitly represented, instead the Ding header type
 - is used.
 -}
module Language.TEI.Syntax
  ( TEI
  , Entry(..)
  , Form(..)
  , Sense(..)
  , Translation(..)
  ) where

import Data.NatLang.Dictionary (Dictionary)
import Data.NatLang.Grammar (GrammarInfo)
import Data.NatLang.Usage (Usage)
import Data.NatLang.InflectedForms (InflectedForms)
import qualified Language.Ding.Syntax as Ding (Header)
import Language.TEI.Syntax.Reference (Ident, Reference)
import Data.NatLang.Example (Example)


-- | A TEI dictionary, with a Ding header.
type TEI = Dictionary Ding.Header Entry


-- | A TEI entry, with a unique identifier, a form (containing the headword,
--   i.a.), grammar annotation and a list of senses (containing translations,
--   i.a.).
--   Note that annotated grammar information should pertain to all senses.
data Entry = Entry
  { entryIdent   :: Ident
  , entryForm    :: Form
  , entryGrammar :: [GrammarInfo]
  , entrySenses  :: [Sense]
  }
 deriving Show


-- | A form, containing a single headword and potentially some related forms.
--   Pronunciation information, that also belongs to a `<form>' is added by a
--   separate tool, after TEI XML generation.
data Form = Form
  { formOrth      :: String   -- ^ headword
  , formAbbrevs   :: [String]
  , formInflected :: Maybe InflectedForms
  }
 deriving (Show, Eq, Ord)


-- | A sense, containing translations and annotations.
data Sense = Sense
  { senseGrammar      :: [GrammarInfo]
  , senseUsages       :: [Usage]
  , senseTranslations :: [Translation]
  , senseExamples     :: [Example]
  , senseReferences   :: [Reference]
  , senseNotes        :: [String]     -- ^ from suffixing Ding-<()>-annotations
  }
 deriving (Show, Eq, Ord)


-- | A translation to a headword, together with annotations.  Corresponds
--   to `<cit type="trans" />'.
data Translation = Translation
  { translationOrth      :: String
  , translationGrammar   :: [GrammarInfo]
  , translationUsages    :: [Usage]
  , translationAbbrevs   :: [String]
  , translationInflected :: Maybe InflectedForms
  , translationNotes     :: [String]
  }
 deriving (Show, Eq, Ord)


-- vi: ft=haskell ts=2 sw=2 et
