{
module JavaScriptParse where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import JavaScript
import Lexer
import Operators
}

%name parser
%tokentype { Token }

%token 
    function { TokenKeyword "function" }
    if    { TokenKeyword "if" }
    else  { TokenKeyword "else" }
    true  { TokenKeyword "true" }
    false { TokenKeyword "false" }
    var   { TokenKeyword "var" }
    try   { TokenKeyword "try" }
    catch { TokenKeyword "catch" }
    undefined { TokenKeyword "undefined" }
    ':'   { Symbol ":" }
    '.'   { Symbol "." }
    ','   { Symbol "," }
    ';'   { Symbol ";" }
    id    { TokenIdent $$ }
    digits { Digits $$ }
    '='    { Symbol "=" }
    '+'    { Symbol "+" }
    '-'    { Symbol "-" }
    '*'    { Symbol "*" }
    '/'    { Symbol "/" }
    '<'    { Symbol "<" }
    '>'    { Symbol ">" }
    '<='   { Symbol "<=" }
    '>='   { Symbol ">=" }
    '=='   { Symbol "==" }
    '&&'   { Symbol "&&" }
    '!'    { Symbol "!" }
    '||'   { Symbol "||" }
    '('    { Symbol "(" }
    ')'    { Symbol ")" }
    '{'    { Symbol "{" }
    '}'    { Symbol "}" }

%%

Exp : Exp2 ';' Exp                      { Seq $1 $3 }
    | var id '=' Exp2 ';' Exp    { Declare $2 $4 $6 }
    | Exp2 				{ $1 }

Exp2 : function '(' OptId ')' '{' Exp '}'  { Function $3 $6 }
    | if '(' Exp2 ')' '{' Exp '}' else '{' Exp '}'  { If $3 $6 $10 }
    | try '{' Exp '}' catch '{' OptExp '}'     { TryCatch $3 $7 }
    | Assign                           { $1 }
 
OptId : id   		  { $1 }
      |                   { "_" }

Assign : Or '=' Assign    { Assign $1 $3 }
       | Or               { $1 }

Or   : Or '||' And        { Binary Or $1 $3 }
     | And                { $1 }

And  : And '&&' Comp      { Binary And $1 $3 }
     | Comp               { $1 }

Comp : Comp '==' Term     { Binary EQ $1 $3 }
     | Comp '<' Term      { Binary LT $1 $3 }
     | Comp '>' Term      { Binary GT $1 $3 }
     | Comp '<=' Term     { Binary LE $1 $3 }
     | Comp '>=' Term     { Binary GE $1 $3 }
     | Term               { $1 }

Term : Term '+' Factor    { Binary Add $1 $3 }
     | Term '-' Factor    { Binary Sub $1 $3 }
     | Factor             { $1 }

Factor : Factor '*' Primary    { Binary Mul $1 $3 }
       | Factor '/' Primary    { Binary Div $1 $3 }
       | Primary               { $1 }

Primary : '-' Primary2    { Unary Neg $2 }
        | '!' Primary2    { Unary Not $2 }
        | Primary2	  { $1 }

Primary2 : digits         { Literal (IntV $1) }
        | true           { Literal (BoolV True) }
        | false          { Literal (BoolV False) }
        | undefined 	 { Literal Undefined }
        | id             { Variable $1 }
        | '(' Exp ')'    { $2 }
        | Primary2 '(' OptExp ')' { Call $1 $3 } 
        | '{'  FieldList  '}'   { Record $2 }
        | Primary2 '.' id   { Field $1 $3 }

FieldList : Field               { [ $1 ] }
        |  Field ',' FieldList  { $1 : $3 }

Field   :  id ':' Exp     { ( $1, $3 ) }


OptExp : Exp   		  { $1 }
       |                  { Literal Undefined }

{

symbols = ["+", "-", "*", "/", "(", ")", "{", "}", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!", "@", ":", ".", ","]
keywords = ["function", "var", "if", "else", "true", "false", "mutable", "undefined", "return", "try", "catch"]
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)

}
