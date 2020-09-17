{-
 - Language/Ding/Enrich/Example/Identify.hs - example identification
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
 - Identify examples.  Both on their own (`isPotentialExample') and
 - in relation to other unit strings (`isExampleOf').
 -}
module Language.Ding.Enrich.Example.Identify
  ( isPotentialExample
  , isExampleOf
  ) where

import Data.Char (isPunctuation)
import Data.List (isInfixOf)


-- | Decide whether a unit string should be considered when searching for
--   examples to a non-example.
isPotentialExample :: String -> Bool
isPotentialExample = (||) <$> isPhrase <*> containsSpecial

-- | Identify non-trivial composed espressions.
isPhrase :: String -> Bool
isPhrase = (>=3) . length . words

-- | Identify unit strings that contain interpunctuation (including quote
--   signs)
containsSpecial :: String -> Bool
containsSpecial = any isPunctuation

-- | Decide whether a potential example should be considered example of a
--   supplied unit string.
--   Any potential example should priorly afore have been identified as such
--   by means of `isPotentialExample'.
isExampleOf :: String -> String -> Bool

-- TODO:
--  * Better example identification, suggestions:
--    * consider umlauts, e.g. "Ball" ~ "zwei große Bälle"
--    * consider change in capitalisation, e.g. "Ball" ~ "ein großer Fußball"
--    * [consider whitespace, e.g. "lang" ~/~ "... langweilig ..."]
isExampleOf = flip isInfixOf

-- Simple way to perform more flexible matching (not very efficient):
--isExampleOf  _  "" = False
--isExampleOf  pex hw = prefixMatch hw ex || hasExample (tail pex) hw
--prefixMatch :: String -> String -> Bool
--prefixMatch = ???


-- vi: ft=haskell ts=2 sw=2 et
