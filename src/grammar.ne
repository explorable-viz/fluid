@preprocessor typescript

@{%
const moo = require('moo')
const lexer = moo.compile({
   ident: {
      match: /[a-zA-Z_][0-9a-zA-Z_]*/, // greedy
      type: moo.keywords({
        keyword: ["as", "match", "fun", "in", "let", "letrec", "primitive", "typematch"],
      })
   },
   WS: {
      match: /[ \t\n]+/, // include \s?
      lineBreaks: true
   },
   comment: /\/\/.*?$/,
   number: /0|[1-9][0-9]*/,
   string: /"(?:\\["\\]|[^\n"\\])*"/,
   // not quite sure why I can't use literals here:
   sumOp: /\-|\+\+|\+/,
   productOp: /\*|\//,
   exponentOp: /\*\*/,
   compareOp: /===|==|<==|<=|<|>==|>=|>/,
   symbol: ["(", ")", "=", "→", ";", "{", "}", ",", "[", "]", "..."], // needs to come after compareOp
})
%}

@lexer lexer

@{%
import { Expr } from "./Expr"
import { ν, num, str } from "./Versioned"
%}

# Match expr with leading whitespace/comments.
rootExpr -> _ expr {% (d: any[]) => d[1] %}

lexeme[X] -> $X _ {% (d: any[]) => d[0] %}
lexeme_[X] -> $X | $X %WS
keyword[X] -> lexeme[$X] # currently no reserved words

expr -> compareExpr
compareExpr -> compareExpr compareOp sumExpr | sumExpr
sumExpr -> sumExpr sumOp productExpr | productExpr
productExpr -> productExpr productOp exponentExpr | exponentExpr
exponentExpr -> exponentExpr exponentOp appChain | appChain

appChain -> simpleExpr | appChain simpleExpr

simpleExpr -> 
   var |
   string |
   number |
   parenthExpr |
   pair |
   defs1 |
   list |
   matchAs |
   fun |
   typematch

var -> lexeme[%ident] {% (d: any[]) => str(ν(), d[0] as string) %}
string -> lexeme[%string] {% (d: any[]) => Expr.constStr(ν(), str(ν(), d[0] as string)) %}
number -> lexeme[number_] {% (d: any[]) => Expr.constNum(ν(), num(ν(), new Number(d[0] as string).valueOf())) %}
parenthExpr -> lexeme["("] expr lexeme[")"] {% (d: any[]) => d[1] %}
pair -> lexeme["("] expr lexeme[","] expr lexeme[")"]
defs1 -> defList keyword["in"] expr
list -> lexeme["["] listOpt lexeme["]"] # ouch: "
typematch -> keyword["typematch"] expr keyword["as"] typeMatches

defList -> def (lexeme[";"] def):*
def -> let | letrec # | prim

let -> keyword["let"] var lexeme["="] expr
letrec -> keyword["letrec"] recDef (lexeme[";"] recDef):*
recDef -> keyword["fun"] var matches
fun -> keyword["fun"] matches
matchAs -> keyword["match"] expr keyword["as"] matches

matches ->
   match |
   lexeme["{"] match (lexeme[";"] match):* lexeme["}"]

match -> 
   pattern lexeme["→"] expr |
   pattern matches

typeMatches ->
   typeMatch |
   lexeme["{"] typeMatch (lexeme[";"] typeMatch):* lexeme["}"]

typeMatch -> lexeme[%ident] lexeme["→"] expr

listOpt -> 
   null |
   expr (lexeme[","] expr):* listRestOpt

listRestOpt ->
   null |
   lexeme[","] lexeme["..."] expr

pattern -> var_pattern | pair_pattern | list_pattern

var_pattern -> var
pair_pattern -> lexeme["("] pattern lexeme[","] pattern lexeme[")"]
list_pattern -> lexeme["["] listOpt_pattern lexeme["]"] # ouch: "

listOpt_pattern -> 
   null | 
   pattern (lexeme[","] pattern):* listRestOpt_pattern

listRestOpt_pattern ->
   null |
   lexeme[","] lexeme["..."] pattern

compareOp -> lexeme_[%compareOp]
exponentOp -> lexeme_[%exponentOp]
productOp -> lexeme_[%productOp]
sumOp -> lexeme_[%sumOp]  

# JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
number_ -> int
int -> [0] | digit1to9 DIGIT:*
digit1to9 -> [1-9]
# I'm assuming (but haven't checked) that DIGIT is defined as
DIGIT -> [0-9]

_ -> (whitespace | singleLineComment):*
whitespace -> [\s]:+
singleLineComment -> "//" [^\n]:*
