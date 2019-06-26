@preprocessor typescript

@{%
const moo = require('moo')
const lexer = moo.compile({
   WS: /[ \t]+/,
   comment: /\/\/.*?$/,
   number: /0|[1-9][0-9]*/,
   string: /"(?:\\["\\]|[^\n"\\])*"/,
   lparen: '(',
   rparen: ')',
   keyword: ['while', 'if', 'else', 'moo', 'cows'],
   NL: { match: /\n/, lineBreaks: true },
   sumOp: /\+|\-|\+\+/,
   productOp: /\*|\//,
   exponentOp: /\*\*/,
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

appChain -> simpleExpr

simpleExpr -> 
   var |
   number |
   parenthExpr |
   defs1 |
   fun

var -> lexeme[[a-zA-Z_] [0-9a-zA-Z_]:*]
number -> lexeme[number_]
parenthExpr -> lexeme["("] expr lexeme[")"]
defs1 -> defList keyword["in"] expr

defList -> def (lexeme[";"] def):*
def -> let # | prim | letrec

let -> keyword["let"] var lexeme["="] expr
fun -> keyword["fun"] matches

matches ->
   match |
   lexeme["{"] match (lexeme[";"] match):* lexeme["}"]

match -> 
   pattern lexeme["→"] expr |
   pattern matches

pattern -> 
   var_pattern

var_pattern -> var

compareOp -> 
   lexeme["=="] | 
   lexeme["==="] | 
   lexeme["<="] | 
   lexeme["<=="] | 
   lexeme["<"] | 
   lexeme[">="] | 
   lexeme[">=="] | 
   lexeme[">"]
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
