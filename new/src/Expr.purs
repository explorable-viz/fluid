module Expr where

import Bindings (Var)
import Selected (Selected(..))

data T3 a b c = T3 a b c

data RawExpr =
    Int Int
  | Var Var
  | True | False
  | Pair Expr Expr
  | Nil | Cons Expr Expr
  | Op Var
  | Let Var Expr Expr
  | Match Expr (Elim Expr)
  | Letrec String (Elim Expr) Expr
  | App Expr Expr
  | BinaryApp Expr Var Expr

data Expr = Expr Selected RawExpr

expr :: RawExpr -> Expr
expr r = Expr Bot r

data Elim k =
     ElimVar Var k
   | ElimPair (Elim (Elim k))
   | ElimList { nil :: k, cons :: Elim (Elim k) }
   | ElimBool { true :: k, false :: k }
