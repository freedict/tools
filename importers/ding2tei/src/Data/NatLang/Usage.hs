{-
 - Data/NatLang/Usage.hs - usage information
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
 - Usage information, as present in both the Ding dictionary and TEI.
 -}
module Data.NatLang.Usage
  ( Usage(..)
  , UsageType(..)
  ) where

-- Notes:
--  * In the Ding, usages are represented within <[]>.
--  * In TEI, usages are represented in <usg> tags.
--  * See the TEI doc on <usg>.
--    * https://www.tei-c.org/release/doc/tei-p5-doc/en/html/DI.html#DITPUS
--  * See also the TEI Lex-0 documentation on <usg>.
--    * https://dariah-eric.github.io/lexicalresources/pages/TEILex0/TEILex0.html#index.xml-body.1_div.7_div.2
--  * See also the Wikipedia on Varieties and Registers (contains a list).
--    * https://en.wikipedia.org/wiki/Variety_(linguistics)
--    * https://en.wikipedia.org/wiki/Register_(sociolinguistics)

-- | Usage information, bearing a particular type.
data Usage = Usage UsageType String
 deriving (Show, Eq, Ord)

-- Maps directly to TEI recommended \@type values for <usg>.
-- | Usage types.
--   These map directly to the TEI Guidelines' recommended \@type value for
--   <usg>.  Some unused types are omitted.
--   See: <https://www.tei-c.org/release/doc/tei-p5-doc/en/html/DI.html#DITPUS>
data UsageType
  = Regional
  | Time
  | Domain
  | Register
  | Style
  | Preference
  | Acceptability   -- likely unused; consider to remove
  | Language        -- @type="lang" - do not confound with @xml:lang !
  | Colloc
  | Hint
 deriving (Eq, Ord)

-- | Show the corresponding TEI recommended values for usage types.
--   See <https://www.tei-c.org/release/doc/tei-p5-doc/en/html/DI.html#DITPUS>.
instance Show UsageType where
  show Regional      = "geo"
  show Time          = "time"
  show Domain        = "dom"
  show Register      = "reg"
  show Style         = "style"
  show Preference    = "plev"
  show Acceptability = "acc"
  show Language      = "lang"
  show Colloc        = "colloc"
  show Hint          = "hint"


-- vi: ts=2 sw=2 et
