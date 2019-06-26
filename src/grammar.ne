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
   WS: /[ \t]+/, // include \s?
   comment: /\/\/.*?$/,
   number: /0|[1-9][0-9]*/,
   string: /"(?:\\["\\]|[^\n"\\])*"/,
   NL: { // needed?
      match: /\n/, 
      lineBreaks: true
   },
   // not quite sure why I can't use literals here:
   sumOp: /\+|\-|\+\+/,
   productOp: /\*|\//,
   exponentOp: /\*\*/,
   compareOp: /==|===|<=|<==|<|>=|>==|>/,
   symbol: ["(", ")", "=", "→", ";", "{", "}", ",", "[", "]"], // needs to come after compareOp
})
%}

@lexer lexer

# Match expr with leading whitespace/comments.
rootExpr -> _ expr

lexeme[X] -> $X _
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
   number |
   parenthExpr |
   pair |
   defs1 |
   list |
   matchAs |
   fun

var -> lexeme[%ident]
number -> lexeme[number_]
parenthExpr -> lexeme["("] expr lexeme[")"]
pair -> lexeme["("] expr lexeme[","] expr lexeme[")"]
defs1 -> defList keyword["in"] expr
list -> lexeme["["] list_ lexeme["]"] # ouch: "

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

list_ -> null | expr (lexeme[","] expr):*

pattern -> var_pattern | pair_pattern

var_pattern -> var
pair_pattern -> lexeme["("] pattern lexeme[","] pattern lexeme[")"]

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
whitespace -> [\s]:+ {% d => null %}
singleLineComment -> "//" [^\n]:* {% d => null %}
