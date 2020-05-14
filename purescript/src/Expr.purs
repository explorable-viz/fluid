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

type Expr = { α :: Selected, r :: RawExpr }

expr :: RawExpr -> Expr
expr r = { α: Bot, r }

data Elim =
     ElimVar { x :: Var, e :: Expr }
   | ElimPair { x :: Var, y :: Var, e:: Expr }
   | ElimList { nil :: Expr, cons :: { x :: Var, y :: Var, e:: Expr } }
   | ElimBool { true :: Expr, false :: Expr }

derive instance eqT3 :: (Eq a, Eq b, Eq c) => Eq (T3 a b c)
instance showT3 :: (Show a, Show b, Show c) => Show (T3 a b c) where
  show (T3 a b c) = "T3 " <> show a <> " " <> show b <> " " <> show c

derive instance eqRawExpr :: Eq RawExpr
derive instance eqElim :: Eq Elim
