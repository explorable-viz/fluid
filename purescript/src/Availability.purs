module Availability where

import Expr

class Available a where
    isTop    :: a -> Boolean
    isBottom :: a -> Boolean

instance exprAvailable :: Available Expr where
    isTop    ExprBottom = false
    isTop    _          = true
    isBottom ExprBottom = false
    isBottom _          = true

instance typAvailable :: Available Typ where
    isTop    TypBottom = false
    isTop    _         = true
    isBottom TypBottom = false
    isBottom _         = true

instance valAvailable :: Available Val where
    isTop    ValBottom = false
    isTop    _         = true
    isBottom ValBottom = false
    isBottom _         = true