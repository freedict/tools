{-
 - Language/Ding/Parser/Line.y - parser for a single regular line
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
--  * The Happy syntax should be very similar to the syntax of Yacc/Bison.
--  * There is an important relation between the lexer (produced with the help
--    of Alex) and the parser.
--  * Happy Documentation: https://www.haskell.org/happy/


{
-- Haskell module header and import statements.

module Language.Ding.Parser.Line (parseLine) where

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NEList

import Data.NatLang.GrammarInfo (GrammarInfo(..))
import Data.NatLang.Grammar (GramLexCategory(..), Case(..))
import Data.NatLang.Usage (Usage)
import Data.NatLang.InflectedForms (InflectedForms(..), InflectedForm(..))
import Language.Ding.Partial.Unit (PartialUnit)
import qualified Language.Ding.Partial.Unit as PU
import Language.Ding.Partial.PseudoUnit (PartialPseudoUnit)
import qualified Language.Ding.Partial.PseudoUnit as PSU
import Language.Ding.Syntax
import Language.Ding.Syntax.Usage (stringToUsage)
import Language.Ding.Token
}


%name parseLine         -- name of the resulting function
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

%token '\n'             { Token _ _ NL }
       '::'             { Token _ _ LangSep }
       '|'              { Token _ _ Vert }
       ';'              { Token _ _ Semi }
       ','              { Token _ _ Comma }
       '~'              { Token _ _ Tilde }
       '+'              { Token _ _ Plus }
       '<>'             { Token _ _ Wordswitch }
       '/'              { Token _ _ StrongSlash }
       '</>'            { Token _ _ WeakSlash }
       '//'             { Token _ _ DoubleSlash }

       '{'              { Token _ _ OBrace }
       '}'              { Token _ _ CBrace }
       '['              { Token _ _ OBracket }
       ']'              { Token _ _ CBracket }
       '('              { Token _ _ OParen }
       ')'              { Token _ _ CParen }
       '<'              { Token _ _ OAngle }
       '>'              { Token _ _ CAngle }
       '</'             { Token _ _ OSlash }
       '/>'             { Token _ _ CSlash }

       tok_slashSpecial { Token _ _ (SlashSpecial _) }
       tok_smiley       { Token _ _ (Smiley _) }
       tok_abbrevSlash  { Token _ _ (AbbrevWithSlash _) }
       tok_abbrevPlural { Token _ _ (AbbrevPlural _) }

       -- Note:
       --  * The grammar annotations are separated because gramCase needs to be
       --    separately available (and Happy tokens may not overlap).
       tok_gramPOS      { Token _ _ (GramKW (PartOfSpeech _)) }
       tok_gramGender   { Token _ _ (GramKW (Gender _)) }
       tok_gramNumber   { Token _ _ (GramKW (Number _)) }
       tok_gramST       { Token _ _ (GramKW SingulareTantum) }
       tok_gramPT       { Token _ _ (GramKW PluraleTantum) }
       tok_gramCase     { Token _ _ (GramKW (Case _)) }

       tok_interrogPron { Token _ _ (IntPronKW _) }
       tok_text         { Token _ _ (Text _) }

%%


-------------------------------------------------------------------------------
-- Grammar specification
-------------------------------------------------------------------------------

-- Notes:
--  * Only a single line is parsed.
--  * Left recursion is recommended by the Happy developers, due to space
--    requirements.
--    * This causes lists to be reversed, which requires re-reversal (or
--      special alternative list structures), which makes the grammar often
--      less readable.
--    * Since all lists are expected to be small, space should not be an issue.
--      * Hence, ignore this.
--    - https://www.haskell.org/happy/doc/html/sec-sequences.html
--    - String-concatenation that uses left-recursion has the same problem.
--      - '<>' when applied to tokens does essentially perform string
--        concatentation.
--        - mconcat can be used instead.


-- Note:
--  * The newline token serves as EOL token, such that there is never an error
--    on end of tokens.
--    * (One could also have given this parser a non-newline terminated stream
--      of tokens.)
line :: { Line }
line : groups '::' groups '\n'          { makeLine $1 $3 }

groups :: { [Group] }
groups : group '|' groups               { $1 : $3 }
       | group                          { $1 : [] }

-- Note:
--  * In some cases, there is exactly one unit, which does not obey to the
--    rules of a proper unit - the headword is enclosed in <()>.
--    * It does however (in practice) obey the rules of a PartialPseudoUnit.
--      * Use these, to ignore the unit.  (TODO: improve)
--    * See also: todo/parsing.paren-units
group :: { Group }
group : units                           { Group $ reverse $1 }
      | {- epsilon -}                   { Group [] }
      | pseudoUnit                      { Group [] }

-- Notes:
--  * Produces the list of units in reverse order.
--    * This is to ease the application of `adjunctToUnit'.
--  * Only accepts a non-empty list of units.  (because of the <;> seps)
units :: { [Unit] }
units : units ';' unit                  { $3 : $1 }
      | units ';' pseudoUnit            { map (PSU.adjunctToUnit $3) $1 }
      | unit                            { $1 : [] }


-------------------------------------------------------------------------------
-- Unit

unit :: { Unit }
unit : parenExps partialUnit            { PU.toUnit $1 $2 }

-- TODO: Do not ignore <>.
pseudoUnit :: { PartialPseudoUnit }
pseudoUnit : pseudoUnit gramAnnot       { $1 `PSU.plusGramAnnot` $2 }
           | pseudoUnit usageAnnot      { $1 `PSU.plusUsageAnnot` $2 }
           | pseudoUnit angleExp        { $1 }
           | parenExp parenExps         { PSU.fromSuffixes ($1 : $2) }

-- Notes:
--  * Any infix annotations are silently dropped (see plusToken).  Prefix
--    annotations are considered invalid and not accepted by the grammar.
--  * As of now, only grammar annotations are recognized.  All other are
--    silently dropped.
--  * Once special suffix recognition is added, these should be considered
--    normal text when occuring initially.  (Similarly for special prefixes.)
--  * Initial abbreviations are equated to regular text, iff they are "single",
--    that is contain no <,> or <;> separated list of abbeviations.
-- TODO: Do not ignore angleExp.
partialUnit :: { PartialUnit }
partialUnit : partialUnit partialUnit1  { $1 `PU.plusToken` $2 }
            | partialUnit gramAnnot     { $1 `PU.plusGramAnnot` $2 }
            | partialUnit usageAnnot    { $1 `PU.plusUsageAnnot` $2 }
            | partialUnit abbrevAnnot   { $1 `PU.plusAbbrevAnnot` $2 }
            | partialUnit inflAnnot     { $1 `PU.plusInflAnnot` $2 }
            | partialUnit parenExp      { $1 `PU.plusSuffix` $2 }
            | partialUnit reference     { $1 `PU.plusRef` $2 }
            | partialUnit partialUnitIgnore   { $1 }
            | partialUnit1              { PU.fromToken $1 }
            | singleAbbrevAnnot         { PU.fromToken $1 }

-- Note: As of now, keywords have no special meaning in top-level text.
--       Hence, treat them as simple text.
partialUnit1 :: { Token }
partialUnit1 : tok_text                 { $1 }
             | gtok_anyKW               { $1 }
             | ','                      { $1 }
             | '+'                      { $1 }
             | '//'                     { $1 }
             | '/'                      { $1 }
             | '</>'                    { $1 }

-- TODO: Emtpy the below list.
partialUnitIgnore :: { () }
partialUnitIgnore : angleExp            { () }
                  | '<>'                { () }
                  | tok_slashSpecial    { () }
                  | tok_smiley          { () }


-------------------------------------------------------------------------------
-- Grammar annotation

gramAnnot :: { NonEmpty GrammarInfo }
gramAnnot : '{' gramAnnot1s '}'           { $2 }

-- Note: <;> and <,> are used with different meanings in this context, but to
--       keep things simple, they are considered equivalent as of now.
gramAnnot1s :: { NonEmpty GrammarInfo }
gramAnnot1s : gramAnnot1 ';' gramAnnot1s  { $1 <> $3 }
            | gramAnnot1                  { $1 }

-- The two rules for CollocCase are necessary to avoid a shift/reduce conflict.
-- The first rule only allows a non-empty list of interrogation pronouns.
-- Note: The usage annotations are dropped for now.  (TODO)
--       It is uncertain whether they can be represented in TEI.
gramAnnot1 :: { NonEmpty GrammarInfo }
gramAnnot1 : gramAnnot2s                  { $1 }
           | interrogProns
             '+' tok_gramCase
             usageAnnots              { CollocCase $1 (tokenToCase $3) :| [] }
           | '+' tok_gramCase
             usageAnnots              { CollocCase [] (tokenToCase $2) :| [] }
           | '+' tok_gramPOS
             usageAnnots              { CollocPOS (tokenToPOS $2)      :| [] }

-- TODO: Consider to add '/' as alternative separator.
gramAnnot2s :: { NonEmpty GrammarInfo }
gramAnnot2s : gramAnnot2 ',' gramAnnot2s  { $1 <| $3 }
            | gramAnnot2                  { $1 :| [] }

-- TODO: Do not drop usageAnnots.
gramAnnot2 :: { GrammarInfo }
gramAnnot2 : gramLexCat usageAnnots       { GramLexCategory $1 }

gramLexCat :: { GramLexCategory }
gramLexCat : gtok_gram                    { tokenToGramLexCat $1 }

interrogProns :: { [String] }
interrogProns : tok_interrogPron ',' interrogProns  { tokenToString $1 : $3 }
              | tok_interrogPron                    { tokenToString $1 : [] }


-------------------------------------------------------------------------------
-- Inflected forms

-- Notes:
--  * If there ever happens to appear a conjugated form that matches a grammar
--    keyword, that keyword must become a MultiKW and accepted individually in
--    both cases.  There would be required some work to avoid ambiguity.
--  * NonEmpty lists are used here.  (:|) constructs a NonEmpty list from a
--    single head element and a regular list.  (<|) joins a single element with
--    a NonEmpty list, just like (:) for normal lists.
--    * Considering the implementation of NonEmpty, using (<|) is not very
--      efficient (decons (:|), cons (:), cons (:|) - instead of just
--      cons (:)).
inflAnnot :: { InflectedForms }
inflAnnot : '{' inflAnnot1s ';' inflAnnot1s '}'   { InflectedForms $2 $4 }

inflAnnot1s :: { NonEmpty InflectedForm }
inflAnnot1s : inflAnnot1 ',' inflAnnot1s  { $1 <| $3 }
            | inflAnnot1                  { $1 :| [] }

inflAnnot1 :: { InflectedForm }
inflAnnot1 : inflAnnot2 usageAnnots     { InflectedForm (tokenToString $1) $2 }

-- Note: most inflected forms consist of a single word.  Not all
--       (e.g., "creeped out").
inflAnnot2 :: { Token }
inflAnnot2 : tok_text inflAnnot2        { $1 <> $2 }
           | tok_text                   { $1 }


-------------------------------------------------------------------------------
-- Abbreviation annotations

-- Note: This rule exists purely to provide a prefix for unit.
singleAbbrevAnnot :: { Token }
singleAbbrevAnnot : '</' abbrevAnnot1 '/>'  { $2 }

abbrevAnnot :: { NonEmpty String }
abbrevAnnot : tok_abbrevSlash           { tokenToString $1 :| [] }
            | tok_abbrevPlural          { (tokenToString $1 ++ "s") :| [] }
            | '</' abbrevAnnot1s '/>'   { $2 }

-- Note: <;> is more frequent.
abbrevAnnot1s :: { NonEmpty String }
abbrevAnnot1s : abbrevAnnot1 ';' abbrevAnnot1s  { tokenToString $1 <| $3 }
              | abbrevAnnot1 ',' abbrevAnnot1s  { tokenToString $1 <| $3 }
              | abbrevAnnot1                    { tokenToString $1 :| [] }

-- Consider content a literal (in particular do not drop any inner annotation).
abbrevAnnot1 :: { Token }
abbrevAnnot1 : abbrevAnnot2 abbrevAnnot1  { $1 <> $2 }
              | abbrevAnnot2              { $1 }

-- Note:
--  * <()>, <[]> are rare here.
--    . Ex.: "/Hg(CNO)2/"
--    . Ex.: "section 15, subsection 3 /s.15[3]/"
--    * Nesting likely does not occur at all, but is considered valid.
abbrevAnnot2 :: { Token }
abbrevAnnot2 : tok_text                 { $1 }
              | gtok_anyKW              { $1 }
              | '+'                     { $1 }
              | '/'                     { $1 }
              | '(' abbrevAnnot2 ')'    { $1 <> $2 <> $3 }
              | '[' abbrevAnnot2 ']'    { $1 <> $2 <> $3 }


-------------------------------------------------------------------------------
-- expressions enclodsed in parentheses

-- Note:
--  * The below rules give (String, Token) for a parenthese expression.
--    The string is the content, the token joins the content with the
--    parentheses (and retains any spacing).

parenExps :: { [(String, Token)] }
parenExps : parenExp parenExps          { $1 : $2 }
          | {- epsilon -}               { [] }

parenExp :: { (String, Token) }
parenExp : '(' parenExp1 ')'            { (tokenToString $2, $1 <> $2 <> $3) }

parenExp1 :: { Token }
parenExp1 : parenExp2 parenExp1         { $1 <> $2 }
parenExp1 : parenExp2                   { $1 }

-- Notes:
--  * Several inner annotations are ignored (on purpose), such as references or
--    <{}>-annotations.
--    * Nonetheless, for example <{}>-annotations should not be simplified
--      (allowed to contain more or less anything) - the content should still
--      be a valid annotation.  I.e., use the grammar rules for those
--      annotations.
--  * Nested <()>-annotations are included literally.
--    * They are rare though.  (Hence the nested (<>) application is ok.)
--  * One might consider to include abbreviations literally.  (TODO?)
--    * This would require a abbrevAnnot'-rule, producing a token instead of
--      a list of Strings.
--      * Alternatively, one might reconstruct an equivalent representation,
--        based on that list.
parenExp2 :: { Token }
parenExp2 : tok_text                    { $1 }
          | gtok_anyKW                  { $1 }
          | reference                   { mempty }
          | gramAnnot                   { mempty }
          | inflAnnot                   { mempty }
          | usageAnnot                  { mempty }
          | abbrevAnnot                 { mempty }
          | '(' parenExp1 ')'           { $1 <> $2 <> $3 }
          | ';'                         { $1 }
          | ','                         { $1 }
          | '+'                         { $1 }
          | '<>'                        { mempty }
          | '/'                         { $1 }
          | '</>'                       { $1 }
          | '//'                        { $1 }


-------------------------------------------------------------------------------
-- Expressions enclosed in angle brackets

angleExp :: { NonEmpty String }
angleExp : '<' angleExp1s '>'            { $2 }

-- Note:
--  * Separation by <;>, <,> is rare (two/one occurrences).
angleExp1s :: { NonEmpty String }
angleExp1s : angleExp1 ';' angleExp1s   { tokenToString $1 <| $3 }
           | angleExp1 ',' angleExp1s   { tokenToString $1 <| $3 }
           | angleExp1                  { tokenToString $1 :| [] }

angleExp1 :: { Token }
angleExp1 : angleExp2 angleExp1         { $1 <> $2 }
          | angleExp2                   { $1 }

-- Notes:
--  * There are nested angle-expressions.  It is unclear how to handle them.
--    Drop for now.  (TODO?)
--    * They are rare (two occurences).
--  * Only one occurence of <()>.  Dropped for now.  (TODO?)
angleExp2 :: { Token }
angleExp2 : tok_text                    { $1 }
          | gtok_anyKW                  { $1 }
          | angleExp                    { mempty }
          | '(' angleExp1 ')'           { mempty }


-------------------------------------------------------------------------------
-- References ("~ref")

-- Note:
--  * Due to the simple current word-matching in the lexer, this might catch
--    unexpected references, such as "~word?". (TODO)
reference :: { String }
reference : '~' reference1              { tokenToString $2 }

reference1 :: { Token }
reference1 : tok_text                   { $1 }
           | gtok_anyKW                 { $1 }


-------------------------------------------------------------------------------
-- Usage annotations

-- Zero or more <[]>-enclosed usage annotations.
-- Unified to a single list (e.g., "[a,b] [c]" -> [a,b,c]).
usageAnnots :: { [Usage] }
usageAnnots : usageAnnot usageAnnots    { NEList.toList $1 ++ $2 }
            | {- epsilon -}             { [] }

-- A single usage annotation (e.g., "[a,b]").
usageAnnot :: { NonEmpty Usage }
usageAnnot : '[' usageAnnot1s ']'       { $2 }

-- Note: <,> and </> are treated as synonyms.  They possibly shouldn't.
usageAnnot1s :: { NonEmpty Usage }
usageAnnot1s : usageAnnot1 ',' usageAnnot1s   { $1 <| $3 }
             | usageAnnot1 '/' usageAnnot1s   { $1 <| $3 }
             | usageAnnot1                    { $1 :| [] }

usageAnnot1 :: { Usage }
usageAnnot1 : usageAnnot2s              { stringToUsage $ tokenToString $1 }

usageAnnot2s :: { Token }
usageAnnot2s : usageAnnot2 usageAnnot2s { $1 <> $2 }
             | usageAnnot2              { $1 }

usageAnnot2 :: { Token }
usageAnnot2 : tok_text                  { $1 }
            | gtok_anyKW                { $1 }
            | ';'                       { $1 }
            | '+'                       { $1 }


-------------------------------------------------------------------------------
-- Grouped simple tokens

gtok_anyKW :: { Token }
gtok_anyKW : gtok_gram                  { $1 }
           | tok_interrogPron           { $1 }

gtok_gram :: { Token }
gtok_gram : tok_gramPOS                 { $1 }
          | tok_gramGender              { $1 }
          | tok_gramNumber              { $1 }
          | tok_gramST                  { $1 }
          | tok_gramPT                  { $1 }
          | tok_gramCase                { $1 }

{

-------------------------------------------------------------------------------
-- Auxiliary Haskell code

parseError :: [Token] -> a
parseError [] = error "Parse error at end of line (missing newline?)."
parseError ((Token _ (Position line col) _):_) =
  error $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ "."


-- Create a line from the left and right side's groups.
-- This is essentially `Data.List.zipWith', but throws an error when the lists
-- have different lengths.  (One could also use zipWithExact from the `safe'
-- package, but this would not give a custom error.
makeLine :: [Group] -> [Group] -> Line
makeLine gs hs = Line $ makeLine' gs hs
 where
  makeLine' :: [Group] -> [Group] -> [Entry]
  makeLine' [] []         = []
  makeLine' (g:gs) (h:hs) = (Entry g h) : makeLine' gs hs
  makeLine' _      _      =
    error "Error: Number of groups does not match on two sides of a line."


tokenToGramLexCat :: Token -> GramLexCategory
tokenToGramLexCat (Token _ _ (GramKW gram)) = gram
tokenToGramLexCat _                         =
  error "Language.Ding.HappyParser: Not a grammar keyword."

tokenToGramAnnot :: Token -> GrammarInfo
tokenToGramAnnot (Token _ _ (GramKW gram)) = GramLexCategory gram
tokenToGramAnnot _                         =
  error "Language.Ding.HappyParser: Not a grammar annotation."

tokenToCase :: Token -> Case
tokenToCase (Token _ _ (GramKW (Case cas))) = cas
tokenToCase _                               =
  error "Language.Ding.HappyParser: Not a case keyword."

tokenToPOS (Token _ _ (GramKW (PartOfSpeech pos))) = pos
tokenToPOS _                                       =
  error "Language.Ding.HappyParser: Not a part of speech keyword."

}


-- vi: ft=haskell ts=2 sw=2 et
