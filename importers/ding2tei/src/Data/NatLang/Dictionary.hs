{-
 - Data.NatLang.Dictionary.hs - general dictionary
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

module Data.NatLang.Dictionary
  ( Dictionary(..)
  , Body(..)
  ) where

import Data.NatLang.Language (Language)

-- | A polymorphic dictionary type, parametrised over the header type and the
--   type of entries/lines.
data Dictionary header entry = Dictionary
  { dictHeader  :: header
  , dictSrcLang :: Language
  , dictTgtLang :: Language
  , dictBody    :: Body entry
  }
 deriving Show

-- | The body of a dictionary, composed of a list of entries/lines.
newtype Body entry = Body [entry]
 deriving Show


-- vi: ft=haskell ts=2 sw=2 et
