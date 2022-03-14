{-
 - Main.hs - main program
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


module Main (main) where

import Prelude hiding (log)
import Control.Monad (when)
import Data.Either (isRight)
import Data.Maybe (fromMaybe, isNothing)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (Handle, hPutStr, hPutStrLn, stdout, stderr)

import App.Ding2TEI (ding2tei)
import Language.Ding.AlexScanner (scan)
import Language.Ding.Enrich (enrichUndirected, enrichDirected)
import Language.Ding.Inverse (inverse)
import Language.Ding.Parser (parse)
import Language.TEI.ToXML (prettyTEI)
import Language.TEI.ToXML.ValidateChar (validateString)


-- Notes:
--  * The input file should be newline terminated, even though in most cases
--    it will not matter.
--    * One might consider adding a final newline, if there is none.
--    * Alex needs terminating whitespace, since the end of input  is not
--      treated as '\n' in right context (unlike the beginning of input).
--      (The whitespace in right context is needed to differentiate different
--      kinds of slashes.)
--  * The input is checked for only containing valid XML characters
--    (validateString).
--    * This is not concerned with potentially to be escaped characters, such
--      as '<', '"'.  These are considered valid here.
--    * There are only some very unusual characters that are not accepted in
--      XML.
--    * This is done here instead at the XML generation state to spot the
--      error early.
--      * The result would most likely be the same; the separators present in
--        the Ding are all valid in XML.


main :: IO ()
main = do
  (opts, mInFile, mOutFile) <- getArgs >>= parseArgs

  when (optPrintHelp opts) $ help stdout >> exitSuccess

  input <- readMFile mInFile
  
  -- TODO: validateString should not `error'.
  let (mDing, log) = parse $ scan $ validateString input

  (if optSkipErrors opts then printLog else printLogAbortOnError)
    (fromMaybe "<stdin>" mInFile) log

  when (isNothing mDing) exitFailure

  -- If non-critical errors are to be skipped, they do not influence the return
  -- code.
  when (optParseOnly opts) exitSuccess

  ding <- maybe (undefined "Unable to parse input.") return mDing
  let ding' = enrichUndirected ding
  let ding'' = enrichDirected $ if optInverse opts then inverse ding else ding'
  let tei = ding2tei ding''
  let outStr = prettyTEI tei
  
  writeMFile mOutFile outStr


readMFile :: Maybe String -> IO String
readMFile Nothing         = getContents
readMFile (Just fileName) = readFile fileName

writeMFile :: Maybe String -> String -> IO ()
writeMFile Nothing         = putStr
writeMFile (Just fileName) = writeFile fileName


-- | Write log up to and including first error message to stderr.
--   Exit with nonzero return code in case of any error message.
printLogAbortOnError :: String -> [Either String String] -> IO ()
printLogAbortOnError inFile log = do
  let (good, afterGood) = span isRight log

  printLog inFile good
  case afterGood of
    (e:_) -> printLog inFile (pure e) >> exitFailure
    []    -> return ()

-- | Write log to stderr.
printLog :: String -> [Either String String] -> IO ()
printLog inFile log =
  hPutStr stderr $ unlines $ map (either (errorPrefix ++) (infoPrefix ++)) log
 where
  errorPrefix  = "Error: " ++ commonPrefix
  infoPrefix   = "Info:  " ++ commonPrefix
  commonPrefix = inFile ++ ": "


data Opts = Opts
  { optInverse    :: Bool
  , optParseOnly  :: Bool
  , optSkipErrors :: Bool
  , optPrintHelp  :: Bool
  }

defaultOpts :: Opts
defaultOpts = Opts
  { optInverse    = False
  , optParseOnly  = False
  , optSkipErrors = False
  , optPrintHelp  = False
  }

parseArgs :: [String] -> IO (Opts, Maybe String, Maybe String)
parseArgs
  = either
      (\e -> hPutStrLn stderr e >> help stderr >> exitFailure)
      return
  . parseArgs'

parseArgs' :: [String] -> Either String (Opts, Maybe String, Maybe String)
parseArgs' allArgs = do
  (opts, fileNames) <- aux defaultOpts allArgs
  case fileNames of
    []                -> return (opts, Nothing, Nothing)
    [inFile]          -> return (opts, maybeFilename inFile, Nothing)
    [inFile, outFile] ->
      if optParseOnly opts
      then fail "Too many non-option arguments."
      else return (opts, maybeFilename inFile, maybeFilename outFile)
    _                 -> Left "Too many non-option arguments."
 where
  aux :: Opts -> [String] -> Either String (Opts, [String])
  aux opts []                       = return (opts, [])
  aux opts ("--"            : args) = return (opts, args)
  aux opts ("-i"            : args) = aux (opts { optInverse    = True }) args
  aux opts ("--inverse"     : args) = aux (opts { optInverse    = True }) args
  aux opts ("--parse-only"  : args) = aux (opts { optParseOnly  = True }) args
  aux opts ("--skip-errors" : args) = aux (opts { optSkipErrors = True }) args
  aux opts ("--validate"    : args) =
    aux (opts { optParseOnly = True, optSkipErrors = True }) args
  aux opts ("-h"            : args) = aux (opts { optPrintHelp  = True }) args
  aux opts ("--help"        : args) = aux (opts { optPrintHelp  = True }) args
  aux _    (o@('-':_:_)     : _   ) = Left $ "Invalid option `" ++ o ++ "'."
  aux opts (str             : args) = (fmap . fmap) (str :) $ aux opts args

  maybeFilename :: String -> Maybe String
  maybeFilename "-" = Nothing
  maybeFilename str = Just str


help :: Handle -> IO ()
help h = do 
  progName <- getProgName
  hPutStr h $ unlines
    [ "Usage: " ++ progName ++ " [OPTION]... [INFILE [OUTFILE]]"
    , ""
    , "When INFILE or OUTFILE is missing or `-', use standard input or output,"
    , "respectively."
    , ""
    , "  -i, --inverse         Invert dictionary direction."
    , "      --parse-only      Only parse the input, do not produce output."
    , "      --skip-errors     Continue on errors where possible."
    , "      --validate        Equivalent to `--parse-only --skip-errors'."
    , "  -h, --help            Print this help message and exit."
    ]


-- vi: ft=haskell ts=2 sw=2 et
