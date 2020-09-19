{-
 - Language/TEI/Syntax/Body.hs - TEI Body
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

module Language.TEI.Syntax.Body
  ( Entry(..)
  , Form(..)
  , Sense(..)
  , Translation(..)
  ) where


import Data.NatLang.GrammarInfo (GrammarInfo)
import Data.NatLang.Usage (Usage)
import Data.NatLang.InflectedForms (InflectedForms)
import Language.TEI.Syntax.Reference (Ident, Reference)
import Language.Common.Syntax (Example)


-- Note:
--  * An entry may group several senses of a word.
--    * Entries being different parts of speech are however to be separated.
--      * Grouping several <entry>'s in a <ubsEntry>, as similarly suggested by
--        TEI Lex-0, is not desired in FreeDict TEI.
data Entry = Entry
  { entryIdent   :: Ident
  , entryForm    :: Form
  , entryGrammar :: [GrammarInfo]
  , entrySenses  :: [Sense]
  }
 deriving Show


-- Notes:
--  * Pronunciation information is added by another tool, after producing TEI
--    XML.
--  * Only a single <orth> element is supported here.
data Form = Form
  { formOrth      :: String   -- ^ headword
  , formAbbrevs   :: [String]
  , formInflected :: Maybe InflectedForms
  }
 deriving (Show, Eq, Ord)


data Sense = Sense
  { senseGrammar      :: [GrammarInfo]
  , senseUsages       :: [Usage]
  , senseTranslations :: [Translation]
  , senseExamples     :: [Example]
  , senseReferences   :: [Reference]
  , senseNotes        :: [String]       -- ^ from Ding-<()>-annotations
  }
 deriving (Show, Eq, Ord)


data Translation = Translation
  { translationOrth    :: String
  , translationGrammar :: [GrammarInfo]
  , translationUsages  :: [Usage]
  }
 deriving (Show, Eq, Ord)


-- vi: ft=haskell ts=2 sw=2 et
