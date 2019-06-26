@preprocessor typescript

expr -> compareExpr
compareExpr -> compareExpr compareOp sumExpr | sumExpr
sumExpr -> sumExpr sumOp productExpr | productExpr
productExpr -> productExpr productOp exponentExpr | exponentExpr
exponentExpr -> exponentExpr exponentOp appChain | appChain

appChain -> simpleExpr

simpleExpr -> 
   variable |
   number |
   parenthExpr

variable -> [a-zA-Z_] [0-9a-zA-Z_]:*

parenthExpr -> "(" _ expr _ ")"

compareOp -> "==" | "===" | "<=" | "<==" | "<" | ">=" | ">==" | ">"
exponentOp -> "**"
productOp -> "*" | "/"
sumOp -> "+" | "-" | "++"

# JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
number -> int
int -> [0] | digit1to9 DIGIT:*
digit1to9 -> [1-9]
# I'm assuming (but haven't checked that) DIGIT is defined as
DIGIT -> [0-9]

_ -> [\s]:* {% d => null %}
