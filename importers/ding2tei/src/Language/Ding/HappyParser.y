{-
 - Language/Ding/HappyParser.y - parser
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


-- Notes:
--  * This file is supposed to be processed by Happy.  It contains both Happy
--    specific syntax and regular Haskell code.  The former is in some ways
--    similar to the latter.  The result of processing with Happy is a regular
--    Haskell source file.
--  * There is an important relation between the lexer (produced with the help
--    of Alex) and the parser.
--  * Happy Documentation: https://www.haskell.org/happy/


{
-- Haskell module header and import statements.

module Language.Ding.HappyParser (parse) where

import Language.Ding.Syntax
import Language.Ding.Syntax.Grammar (GrammarAnnotation)
import Language.Ding.Token
import Text.ShowEssential (showEssential)
}


%name parse             -- name of the resulting function
%tokentype { Token }
%error { parseError }   -- name of the error function - must be defined later


-- Link Happy tokens (left) to Haskell patterns (right).
--
-- Notes:
--  * If `$$' is specified on the right, the Happy token will evaluate to the
--    token's part matched by `$$'.  Otherwise, the Happy token evaluates to
--    the whole token.
--  * It is common for simple tokens to use as name the string representing the
--    token, however not at all necessary.

%token '{'           { Token _ _ (Separator "{") }
       '}'           { Token _ _ (Separator "}") }
       '('           { Token _ _ (Separator "(") }
       ')'           { Token _ _ (Separator ")") }
       '['           { Token _ _ (Separator "[") }
       ']'           { Token _ _ (Separator "]") }
       '</'          { Token _ _ OSlash }
       '/>'          { Token _ _ CSlash }
       '<'           { Token _ _ (Separator "<") }
       '>'           { Token _ _ (Separator ">") }
       '<>'          { Token _ _ (Separator "<>") }
       '::'          { Token _ _ (Separator "::") }
       ':'           { Token _ _ (Separator ":") }
       '|'           { Token _ _ (Separator "|") }
       ';'           { Token _ _ (Separator ";") }
       ','           { Token _ _ (Separator ",") }
       '~'           { Token _ _ (Separator "~") }
       '#'           { Token _ _ (Separator "#") }
       '//'          { Token _ _ (Separator "//") }
       '/'           { Token _ _ StrongSlash }
       '</>'         { Token _ _ WeakSlash }
       '\n'          { Token _ _ NL }
       '\n>>'        { Token _ _ LineCont }
       slashSpecial  { Token _ _ (SlashSpecial _) }
       smiley        { Token _ _ (Smiley _) }
       slashExp      { Token _ _ (SlashExp _) }
       slashExpPl    { Token _ _ (SlashExpPlural _) }
       gramKW        { Token _ _ (Keyword (GramKW _)) }
       versionPref   { Token _ _ (HeaderPrefix VersionPref) }
       copyrightPref { Token _ _ (HeaderPrefix CopyrightPref) }
       licensePref   { Token _ _ (HeaderPrefix LicensePref) }
       urlPref       { Token _ _ (HeaderPrefix URLPref) }
       url           { Token _ _ (URL _) }
       text          { Token _ _ (Text _) }

%%


-------------------------------------------------------------------------------
-- Grammar specification
-------------------------------------------------------------------------------

-- Notes:
--  * Left recursion is recommended by the Happy developers, due to space
--    requirements.  This causes lists to be reversed.
--    - https://www.haskell.org/happy/doc/html/sec-sequences.html
--    - The simplest solution is to reverse any such list once fully parsed.
--    - One may likely ignore this for small lists, that is, all except the
--      list of lines.
--      - It might be argued that the order of lines does not matter.  I
--        prefer to keep the represention of the dictionary quite close on the
--        first level though.
--    - String-concatenation that uses left-recursion has the same problem.
--      - '<>' when applied to tokens does essentially perform string
--        concatentation.
--        - mconcat can be used instead.
--  * A line continuation does not syntactically continue a line, it instead
--    syntactically forms its own line, which then semantically continues the
--    preceding one.


dict :: { Dict }
dict : header lines        { Dict $1 $ reverse $2 }
     | header lines '\n'   { Dict $1 $ reverse $2 }


-------------------------------------------------------------------------------
-- Header

-- Note: The header of the es-de dictionary is specified differently.
header :: { Header }
header : versionHeader
         copyrightHeader
         licenseHeader
         urlHeader          { Header { headerVersion         = fst $1
                                     , headerVersionDate     = snd $1
                                     , headerCopyrightHolder = fst $2
                                     , headerCopyrightPeriod = snd $2
                                     , headerLicense         = $3
                                     , headerURL             = $4
                                     }
                            }

versionHeader :: { (String, String) }
versionHeader : versionPref text '\n'   { let [version, date] = words $
                                                showEssential $2
                                          in  (version, date) }

copyrightHeader :: { (String, String) }
copyrightHeader : copyrightPref copyrightHolder ',' '\n'
                  '#' copyrightPeriod '\n'                { ($2, $6) }

-- TODO: Handling of <...>.  (Currently silently dropped)
copyrightHolder :: { String }
copyrightHolder : text '<' text '>'     { showEssential $1 }

copyrightPeriod :: { String }
copyrightPeriod : text                  { showEssential $1 }

licenseHeader :: { String }
licenseHeader : licensePref text ';' text '\n' { showEssential $
                                                   $2 <> $3 <> $4 }

urlHeader :: { String }
urlHeader : urlPref url '\n'            { showEssential $2 }


-------------------------------------------------------------------------------
-- Ordinary lines

-- Note: The list of lines is reversed.
lines :: { [Line] }
lines : lines '\n' line            { $3 : $1 }
      | line                       { $1 : [] }

-- A line composed of an initial "real" line and possibly several line
-- continuations.  The latter are sequentially appended.
line :: { Line }
line : line '\n>>' realLine             { $1 <> $3 }
     | realLine                         { $1 }

-- A string of characters, delimited by newlines.
-- TODO: Consider to use Safe.Exact.zipWithExact or similar.
--       (Raise error on different list lengths.)
--       For now, there is `testsuite/test-ding.sed', which checks for a
--       matching group count (It is not necessarily correct though).
--       Alternatively, pass the number of groups read on the left side
--       to the right.  Use a monad (or an expression grammar).
--        - See Happy doc.
realLine :: { Line }
realLine : groups '::' groups         { Line $ reverse $ zipWith Entry $1 $3 }

groups :: { [Group] }
groups : groups '|' group               { $3 : $1 }
       | group                          { $1 : [] }

group :: { Group }
group : units                           { Group $ reverse $1 }

units :: { [Unit] }
units : units ';' unit                  { $3 : $1 }
      | unit                            { $1 : [] }

unit :: { Unit }
unit : partialUnit                      { toUnit $1 }
     | {- epsilon -}                    { NullUnit }

-- Notes:
--  * Any infix annotations are silently dropped (see plusToken).  Prefix
--    annotations are considered invalid and not accepted by the grammar.
--  * As of now, only grammar annotations are recognized.  All other are
--    silently dropped.
partialUnit :: { PartialUnit }
partialUnit : partialUnit partialUnit1  { $1 `plusToken` $2 }
            -- | partialUnit gramAnnot     { $1 `plusAnnot` $2 }
            -- | partialUnit conjAnnot     { $1 }
            | partialUnit1              { fromToken $1 }

partialUnit1 :: { Token }
partialUnit1 : topText1                 { $1 }


topText :: { Token }
topText : topText1s                     { mconcat $ reverse $1 }

topText1s :: { [Token] }
topText1s : topText1s topText1          { $2 : $1 }
          | topText1                    { $1 : [] }

-- Note: As of now, keywords have no special meaning in top-level text.
--       Hence, treat them as simple text.
topText1 :: { Token }
topText1 : text                         { $1 }
         | anyKW                        { $1 }
         | topIgnoredSep                { $1 }
         | ignoredAnnot                 { mempty }


anyKW :: { Token }
anyKW : gramKW                          { $1 }

gramAnnot :: { [GrammarAnnotation] }
gramAnnot : '{' gramAnnot1s '}'         { reverse $2 }

-- Note: <;> and <,> are used with different meanings in this context, but to
--       keep things simple, they are considered equivalent as of now.
gramAnnot1s :: { [GrammarAnnotation] }
gramAnnot1s : gramAnnot1s ';' gramAnnot1  { $3 : $1 }
            | gramAnnot1s ',' gramAnnot1  { $3 : $1 }
            | gramAnnot1                  { $1 : [] }

gramAnnot1 :: { GrammarAnnotation }
gramAnnot1 : gramKW                     { tokenToGramAnnot $1 }


-- Notes:
--  * If there ever happens to appear a conjugated form that matches a grammar
--    keyword, that keyword must become a MultiKW and accepted individually in
--    both cases.  There would be required some work to avoid ambiguity.
--  * As of now, any inner annotations (e.g. "[obs.]") are silently dropped
--    (see conjAnnot2).
conjAnnot : '{' conjAnnot1 ';' conjAnnot1 '}'   { ($3, $5) }

conjAnnot1 : conjAnnot2s                  { reverse $1 }

conjAnnot2s : conjAnnot2s ',' conjAnnot2  { $3 : $1 }
            | conjAnnot2                  { $1 : [] }

conjAnnot2 : conjAnnot2 anyAnnot          { $1 }
           | text                         { showEssential $1 }

--slashAnnot :: { SlashAnnot }
--slashAnnot : slashExp                   { SlashAnnot True $1 }
--           | '</' abbrevs '/>'          { SlashAnnot False $ showEssential $2 }
--
--abbrevs :: { [String] }
--abbrevs : abbrevs ';' abbrev            { $3 : $1 }
--
--abbrev :: { String }
--abbrev : {- TODO -}                     { "TODO" }

-- Note: This does not include any kind of brackets or newlines.
anySep :: { Token }
anySep : '<>'                           { $1 }
       | '::'                           { $1 }
       | ':'                            { $1 }
       | '|'                            { $1 }
       | ';'                            { $1 }
       | ','                            { $1 }
       | '~'                            { $1 }
       | '//'                           { $1 }
       | '/'                            { $1 }
       | '</>'                          { $1 }
       | '#'                            { $1 }

topIgnoredSep : '<>'                    { $1 }
              | ':'                     { $1 }
              | ','                     { $1 }
              | '~'                     { $1 }
              | '//'                    { $1 }
              | '/'                     { $1 }
              | '</>'                   { $1 }
              | '#'                     { $1 }

innerText :: { () }
innerText : innerText innerText1        { () }
          | {- empty -}                 { () }

innerText1 :: { () }
innerText1 : text                       { () }
           | anyKW                      { () }
           | anySep                     { () }
           | anyAnnot                   { () }

slashText :: { () }
slashText : slashText slashText1        { () }
          | slashText1                  { () }

slashText1 : text                       { () }
           | anyKW                      { () }
           | topIgnoredSep              { () }
           | ';'                        { () }
           | anyAnnot                   { () }

anyAnnot :: { () }
--anyAnnot : '{' innerText '}'            { () }
--         | ignoredAnnot                 { () }
anyAnnot : ignoredAnnot                 { () }

-- Note: Due the large number of inconsistencies in slash-usage in the Ding,
--       do not allow any above-unit separators, to avoid slash annotations
--       spanning multiple units.
ignoredAnnot :: { () }
ignoredAnnot : '{' innerText '}'        { () }
             | '(' innerText ')'        { () }
             | '[' innerText ']'        { () }
             | '<' innerText '>'        { () }
             | '</' slashText '/>'      { () }
             | slashSpecial             { () }
             | smiley                   { () }
             | slashExp                 { () }
             | slashExpPl               { () }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at end of input"
parseError ((Token _ (Position line col) _):_) =
  error $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ "."

data PartialUnit = PartialUnit [Token] [[GrammarAnnotation]]

fromToken :: Token -> PartialUnit
fromToken t = PartialUnit [t] []

plusToken :: PartialUnit -> Token -> PartialUnit
plusToken (PartialUnit ts _) t = PartialUnit (t:ts) []

plusAnnot :: PartialUnit -> [GrammarAnnotation] -> PartialUnit
plusAnnot (PartialUnit ts ass) as = PartialUnit ts (as : ass)

toUnit :: PartialUnit -> Unit
toUnit (PartialUnit ts ass) =
  Unit (showEssential $ mconcat $ reverse ts) (concat $ reverse ass)

tokenToGramAnnot :: Token -> GrammarAnnotation
tokenToGramAnnot (Token _ _ (Keyword (GramKW gram))) = gram
tokenToGramAnnot _                                   =
  error "Language.Ding.HappyParser: Not a grammar annotation."
}

-- vi: ft=haskell ts=2 sw=2 et
