@preprocessor typescript

expr -> compareExpr
compareExpr -> compareExpr compareOp sumExpr | sumExpr
sumExpr -> sumExpr sumOp productExpr | productExpr
productExpr -> productExpr productOp exponentExpr | exponentExpr
exponentExpr -> exponentExpr exponentOp appChain | appChain

appChain -> simpleExpr

simpleExpr -> 
   variable |
   parenthExpr

variable -> [a-zA-Z_] [0-9a-zA-Z_]:*

parenthExpr ->
   "(" _ expr _ ")"

compareOp -> "==" | "===" | "<=" | "<==" | "<" | ">=" | ">==" | ">"
exponentOp -> "**"
productOp -> "*" | "/"
sumOp -> "+" | "-" | "++"

_ -> [\s]:* {% d => null %}
