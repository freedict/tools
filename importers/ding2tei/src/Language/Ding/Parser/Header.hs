{-
 - Language/Ding/Parser/Header.hs - parse the Ding header
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

module Language.Ding.Parser.Header (parseHeader) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import Language.Ding.Syntax (Header(..))


-- | Parse the Ding header from a list of lines.
--   The expected syntax is very strict; it does for example not fit the
--   header of the es-de Ding dictionary.
parseHeader :: [String] -> Header
parseHeader (versionL : copyrightL : yearsL : licenseL : urlL : []) =
  fromMaybe (error "Error: Failed parsing header.") $
    do
      versionInfo <- stripPrefix "# Version :: " versionL
      (version, date) <- case words versionInfo of
        [a, b] -> Just (a, b)
        _      -> Nothing

      copyrightHolder <- stripPrefix "# Copyright (c) :: " copyrightL >>=
                         stripSuffixChar ','

      years <- stripPrefix "# " yearsL

      license <- stripPrefix "# License :: " licenseL

      url <- stripPrefix "# URL :: " urlL

      return $ Header
        { headerVersion = version
        , headerVersionDate = date
        , headerCopyrightHolder = copyrightHolder
        , headerCopyrightPeriod = years
        , headerLicense = license
        , headerURL = url
        }

parseHeader _ = error $ "Error: Incorrect number of header lines."


-- | Attempt to strip off a particular terminating char.
--   Return `Just prefix', where `prefix' is the remaing prefix in case of
--   success and `Nothing' otherwise.
stripSuffixChar :: Char -> String -> Maybe String
stripSuffixChar _ []     = Nothing
stripSuffixChar k [c]    = if k == c then Just [] else Nothing
stripSuffixChar k (c:cs) = fmap (c:) $ stripSuffixChar k cs


-- vi: ts=2 sw=2 et
