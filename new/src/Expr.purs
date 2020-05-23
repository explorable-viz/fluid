module Expr where

import Prelude ((<>))
import Data.Eq (class Eq)
import Data.Show
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
  | Match Expr Elim
  | Letrec String Elim Expr
  | App Expr Expr
  | BinaryApp Expr Var Expr

data RawExpr2 =
    Int2 Int
  | Var2 Var
  | True2 | False2
  | Pair2 Expr2 Expr2
  | Nil2 | Cons2 Expr2 Expr2
  | Op2 Var
  | Let2 Var Expr2 Expr2
  | Match2 Expr2 (Elim2 Expr2)
  | Letrec2 String (Elim2 Expr2) Expr2
  | App2 Expr2 Expr2
  | BinaryApp2 Expr2 Var Expr2

type Expr = { α :: Selected, r :: RawExpr }

type Expr2 = { α :: Selected, r :: RawExpr2 }

expr :: RawExpr -> Expr
expr r = { α: Bot, r }

data Elim =
     ElimVar { x :: Var, e :: Expr }
   | ElimPair { x :: Var, y :: Var, e:: Expr }
   | ElimList { nil :: Expr, cons :: { x :: Var, y :: Var, e:: Expr } }
   | ElimBool { true :: Expr, false :: Expr }

data Elim2 k =
     ElimVar2 Var k
   | ElimPair2 (Elim2 (Elim2 k))
   | ElimList2 { nil :: k, cons :: Elim2 (Elim2 k) }
   | ElimBool2 { true :: k, false :: k }

derive instance eqT3 :: (Eq a, Eq b, Eq c) => Eq (T3 a b c)
instance showT3 :: (Show a, Show b, Show c) => Show (T3 a b c) where
  show (T3 a b c) = "T3 " <> show a <> " " <> show b <> " " <> show c

derive instance eqRawExpr :: Eq RawExpr
derive instance eqElim :: Eq Elim
