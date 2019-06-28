@preprocessor typescript

@{%
const moo = require('moo')
const lexer = moo.compile({
   ident: {
      match: /[a-zA-Z_][0-9a-zA-Z_]*'*/, // greedy
      type: moo.keywords({
        keyword: ["as", "match", "fun", "in", "let", "letrec", "primitive", "typematch"],
      })
   },
   whitespace: {
      match: /[ \f\t\r\n]+/,
      lineBreaks: true
   },
   singleLineComment: /\/\/.*$/,
   // WIP: JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
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
import { List } from "./BaseTypes"
import { Expr } from "./Expr"
import { ν, num, str } from "./Versioned"
%}

# Allow leading whitespace/comments.
rootExpr -> 
   _ expr {% ([, e]) => e %} |
   expr {% id %}

lexeme[X] ->
   $X {% id %} | 
   $X _ {% ([x, ]) => x %}
keyword[X] -> lexeme[$X]

_ -> (%whitespace | %singleLineComment):+

expr -> 
   compareExpr {% id %} |
   defs1 {% id %} |
   fun {% id %}

defs1 ->
   defList keyword["in"] expr {% ([defs, , e]) => Expr.defs(ν(), defs, e) %}

compareExpr -> 
   compareExpr compareOp sumExpr {% ([e1, op, e2]) => Expr.binaryApp(ν(), e1, str(ν(), op), e2) %} | 
   sumExpr {% id %}
sumExpr -> 
   sumExpr sumOp productExpr {% ([e1, op, e2]) => Expr.binaryApp(ν(), e1, str(ν(), op), e2) %} | 
   productExpr {% id %}
productExpr -> 
   productExpr productOp exponentExpr {% ([e1, op, e2]) => Expr.binaryApp(ν(), e1, str(ν(), op), e2) %} |
   exponentExpr {% id %}
exponentExpr -> 
   exponentExpr exponentOp appChain {% ([e1, op, e2]) => Expr.binaryApp(ν(), e1, str(ν(), op), e2) %} |
   appChain {% id %}

appChain -> 
   simpleExpr {% id %} |
   appChain simpleExpr {% ([e1, e2]) => Expr.app(ν(), e1, e2) %}

simpleExpr ->
   variable {% id %} |
   string {% id %} |
   number {% id %} |
   parenthExpr {% id %} |
   pair {% id %} |
   list {% id %} |
   matchAs {% id %} |
   typematch {% id %}

variable -> var {% ([x]) => Expr.var_(ν(), x) %}
var -> lexeme[%ident] {% ([x]) => str(ν(), x as string) %}
string -> lexeme[%string] {% ([lit]) => Expr.constStr(ν(), str(ν(), lit as string)) %}
number -> lexeme[%number] {% ([lit]) => Expr.constNum(ν(), num(ν(), new Number(lit as string).valueOf())) %}
parenthExpr -> lexeme["("] expr lexeme[")"] {% ([, e,]) => e %}
pair -> lexeme["("] expr lexeme[","] expr lexeme[")"]
list -> lexeme["["] listOpt lexeme["]"] # ouch: "
typematch -> keyword["typematch"] expr keyword["as"] typeMatches

defList -> 
   def (lexeme[";"] def {% ([, def]) => def %}):* 
   {% ([def, defs]) => [def, ...defs] %}
def -> let {% id %} | letrec {% id %} | prim {% id %}

let -> 
   keyword["let"] var lexeme["="] expr 
   {% ([, x, , e]) => Expr.let_(x, e) %}
letrec -> 
   keyword["letrec"] recDef (lexeme[";"] recDef {% ([, recDef]) => recDef %}):* 
   {% ([, recDef, δ]) => Expr.letRec(List.fromArray([recDef, ...δ])) %}
prim -> keyword["primitive"] var

recDef -> 
   keyword["fun"] var matches
   {% ([, f, σ]) => Expr.recDef(f, σ) %}

fun -> 
   keyword["fun"] matches
   {% ([, σ]) => Expr.fun(ν(), σ) %}
matchAs -> 
   keyword["match"] expr keyword["as"] matches
   {% ([, e, , σ]) => Expr.matchAs(ν(), e, σ) %}

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

pattern -> 
   var_pattern | 
   pair_pattern | 
   list_pattern

var_pattern -> var
pair_pattern -> lexeme["("] pattern lexeme[","] pattern lexeme[")"]
list_pattern -> lexeme["["] listOpt_pattern lexeme["]"] # ouch: "

listOpt_pattern -> 
   null | 
   pattern (lexeme[","] pattern):* listRestOpt_pattern

listRestOpt_pattern ->
   null |
   lexeme[","] lexeme["..."] pattern

compareOp -> lexeme[%compareOp] {% id %}
exponentOp -> lexeme[%exponentOp] {% id %}
productOp -> lexeme[%productOp] {% id %}
sumOp -> lexeme[%sumOp] {% id %}
