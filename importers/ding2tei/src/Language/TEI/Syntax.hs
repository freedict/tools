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

module Language.TEI.Syntax (TEI) where

import Data.NatLang.Dictionary (Dictionary)
import qualified Language.Ding.Syntax as Ding (Header)
import Language.TEI.Syntax.Body (Entry)


-- Notes:
--  * The TEI header is not explicitly represented as AST; the Ding header is
--    used instead and to be directly translated to TEI XML.
--  * Only a subset of TEI supported, sufficient to represent the Ding.
--    * In particular, only dictionaries can be represented.
--  * Trying to adhere to TEI Lex-0.
--  * The data types' names mostly map to the respective TEI XML node names.


type TEI = Dictionary Ding.Header Entry


-- vi: ft=haskell ts=2 sw=2 et
