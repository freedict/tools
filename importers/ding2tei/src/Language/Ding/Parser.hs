{-
 - Language/Ding/Parser.hs - parser
 -
 - Copyright 2020-2022 Einhard Leichtfuß
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
 - Parse the Ding dictionary from a list of tokens as identified by
 - `Language.Ding.AlexScanner'.  The header is parsed manually, the body by
 - a `Happy' generated module.
 -}
module Language.Ding.Parser (parse) where

import Prelude hiding (log)
import Control.Monad (guard)
import Control.Monad.Writer (runWriter, Writer, mapWriterT)
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (catMaybes)

import Data.NatLang.Dictionary (Dictionary(Dictionary), Body(Body))
import Data.NatLang.Language (Language(German, English))
import Language.Ding.Parser.Header (parseHeader)
import Language.Ding.Parser.Line (parseLine, FailWriter)
import Language.Ding.Syntax (Ding, Line)
import Language.Ding.Token (Token(..), Atom(..))


-- Note:
--  * The parse log is completely separated from the returned value.
--    * Consequence: Writing both in parallel (to stderr and, e.g., a file) is
--      difficult.
--    * This should not affect performance too much, however.  The parse log is
--      usually short and we cannot begin writing the data before most of the
--      parsing is done, anyways.


-- | Construct a Ding AST from a list of tokens, plus a parse log composed of
--   parse errors and notes.
--   Iff one of the errors prevents constructing a valid Ding AST, `Nothing' is
--   returned as such.
parse :: [Token] -> (Maybe Ding, [Either String String])
parse ts =
  ( do
      header <- either (const Nothing) Just eHeader
      guard $ not $ null bodyLines
      return $ Dictionary header German English (Body bodyLines)
  , concat
      [ either (pure . Left) (const []) eHeader
      , bodyLog
      -- Note: Appending a single value is not optimal.  Prepending instead
      --       would yield an unusual order.
      , if null bodyLines
        -- The FreeDict XML schema requires at least one entry and no such can
        -- be generated from nothing.
        then pure $ Left "Input contains no valid dictionary entries."
        else []
      ]
  )
 where
  (headerLines, bodyToks) = separateHeaderLines ts

  eHeader = parseHeader headerLines
  (bodyLines, bodyLog) = runWriter $ parseBody bodyToks


-- | Parse a list of `Line's.
parseBody :: [Token] -> ParseWriter [Line]
parseBody = fmap catMaybes . mapM (mergeErrorToLog . parseLine) . tokLines

-- | A writer logging both informative messages (`Left') and error messages
--   (`Right').
type ParseWriter = Writer [Either String String]

-- | Convert a `FailWriter' to a `ParseWriter'.
--   If an error occurred in the `FailWriter', the `ParseWriter' returns
--   `Nothing' as value.
mergeErrorToLog :: FailWriter a -> ParseWriter (Maybe a)
mergeErrorToLog = mapWriterT aux
 where
  aux :: Either String (a, [String])
      -> Identity (Maybe a, [Either String String])
  aux (Left e)         = Identity (Nothing, pure $ Left e)
  aux (Right (x, log)) = Identity (Just x,  map Right log)


-- | Separate the initial lines belonging to the header from the body lines.
--   Header lines are converted to strings.
separateHeaderLines :: [Token] -> ([String], [Token])
separateHeaderLines (Token _ _ (HeaderLine hl) : Token _ _ NL : tls) =
  let (hls, tls') = separateHeaderLines tls
  in  (hl : hls, tls')
separateHeaderLines tls = ([], tls)


-- | Break a list of tokens into a list of lines, the latter being also a list
--   of tokens.  The separating newline tokens are retained.  The list of
--   tokens is expected to be newline-terminated.
--   Similar to `Data.OldList.lines', but on tokens, with separator
--   `Language.Ding.Token.NL'.
tokLines :: [Token] -> [[Token]]
tokLines [] = []
tokLines ts =
  let (l, ts') = breakLine ts
  in  l : tokLines ts'


-- | Separate a single line, including newline token from the stream.
--   Requires a newline token to be present.
breakLine :: [Token] -> ([Token], [Token])
breakLine (nl@(Token _ _ NL) : ts) = ([nl], ts)
breakLine (t                 : ts) =
  let (l, ts') = breakLine ts
  in  (t : l, ts')
breakLine []                       =
  -- TODO: Encapsulate error in the logging monad.
  error "Input is not newline-terminated."


-- vi: ts=2 sw=2 et
