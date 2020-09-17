{-
 - Language/Ding/Enrich.hs - enrich the Ding AST
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
 - Enrich the Ding AST with several information.
 - This partially starts a translation towards TEI.
 -}
module Language.Ding.Enrich
  ( enrichUndirected
  , enrichDirected
  ) where

import Language.Ding.Enrich.Example (inferExamples)
import Language.Ding.Syntax (Ding, )
import Data.NatLang.Dictionary (Dictionary(..), Body(..))

-- | Enrich the Ding AST in a way that does not depend on the order of
--   languages.
--   That is, `enrichUndirected . inverse = inverse . enrichUndirected'.
enrichUndirected :: Ding -> Ding
enrichUndirected = id

-- | Enricht the Ding AST considering it a directed dictionary, with a source
--   and target language.
--   This function affects elements in the source and target language
--   differently.
enrichDirected :: Ding -> Ding
enrichDirected (Dictionary header srcLang tgtLang (Body ls))
  = Dictionary header srcLang tgtLang $ Body $ map inferExamples ls


-- vi: ft=haskell ts=2 sw=2 et
