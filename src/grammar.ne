@preprocessor typescript

expr -> compareExpr
compareExpr -> compareExpr compareOp sumExpr | sumExpr
sumExpr -> sumExpr sumOp productExpr | productExpr
productExpr -> productExpr productOp exponentExpr | exponentExpr
exponentExpr -> exponentExpr exponentOp appChain | appChain

compareOp -> "==" | "===" | "<=" | "<==" | "<" | ">=" | ">==" | ">"
exponentOp -> "**"
productOp -> "*" | "/"
sumOp -> "+" | "-" | "++"
