{-
 - Data/NatLang/GrammarInfo.hs - Grammar information
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


module Data.NatLang.GrammarInfo
  ( GrammarInfo(..)
  ) where

import Data.NatLang.Grammar

data GrammarInfo
  = GramLexCategory GramLexCategory
  | CollocCase
      [String]                      -- ^ interrogative pronouns; often none
      Case                          -- ^ case of collocating word
  | CollocPOS       PartOfSpeech    -- ^ POS of collocating word; rare
 deriving (Show, Eq, Ord)


-- vi: ft=haskell ts=2 sw=2 et
