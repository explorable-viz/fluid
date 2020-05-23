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

-- derive instance eqT3 :: (Eq a, Eq b, Eq c) => Eq (T3 a b c)
-- instance showT3 :: (Show a, Show b, Show c) => Show (T3 a b c) where
--   show (T3 a b c) = "T3 " <> show a <> " " <> show b <> " " <> show c

-- derive instance eqRawExpr :: Eq RawExpr
-- derive instance eqElim :: Eq Elim
