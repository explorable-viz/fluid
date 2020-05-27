module Expr where

import Data.List (List)
import Bindings (Var)
import Selected (Selected(..))

data T3 a b c = T3 a b c

-- recursive functions
data Def = Def Var (Elim Expr)
type Defs = List Def

data RawExpr =
    Var Var
  | Op Var
  | Int Int
  | True | False
  | Pair Expr Expr
  | Nil | Cons Expr Expr
  | Lambda (Elim Expr)
  | App Expr Expr
  | BinaryApp Expr Var Expr
  | Match Expr (Elim Expr)
  | Let Var Expr Expr
  | Letrec Defs Expr

data Expr = Expr Selected RawExpr

expr :: RawExpr -> Expr
expr r = Expr Bot r

data Elim k =
     ElimVar Var k
   | ElimPair (Elim (Elim k))
   | ElimList { nil :: k, cons :: Elim (Elim k) }
   | ElimBool { true :: k, false :: k }
