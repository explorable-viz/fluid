module Expr where

import Prelude ((<>))
import Data.Eq (class Eq)
import Data.Show
import Bindings (Bindings, Var)

data T3 a b c = T3 a b c

type Ctx = Bindings Typ

data Availability = Top | Bottom

data Typ = TypBottom
         | TypInt
         | TypBool
         | TypFun Typ Typ
         | TypList Typ
         | TypPair Typ Typ
         | TypFailure String

data Expr = ExprInt Int | ExprIntSel Int
          | ExprVar Var
          | ExprTrue | ExprTrueSel
          | ExprFalse | ExprFalseSel
          | ExprPair Expr Expr | ExprPairSel Expr Expr
          | ExprNil | ExprNilSel
          | ExprCons Expr Expr | ExprConsSel Expr Expr
          | ExprLet Var Expr Expr
          | ExprMatch Expr Elim
          | ExprLetrec String Elim Expr
          | ExprApp Expr Expr
          | ExprAdd Expr Expr

data Elim = ElimVar { x :: Var, tx :: Typ, e :: Expr }
          | ElimPair { x :: Var, tx :: Typ, y :: Var, ty :: Typ, e:: Expr }
          | ElimList { bnil :: Expr, bcons :: { x :: Var, y :: Var, e:: Expr } }
          | ElimBool { btrue :: Expr, bfalse :: Expr }



derive instance eqT3 :: (Eq a, Eq b, Eq c) => Eq (T3 a b c)
instance showT3 :: (Show a, Show b, Show c) => Show (T3 a b c) where
  show (T3 a b c) = "T3 " <> show a <> " " <> show b <> " " <> show c

derive instance eqAvailability :: Eq Availability
instance showAvailability :: Show Availability where
  show Top    = "Top"
  show Bottom = "Bottom"

derive instance eqTyp :: Eq Typ

derive instance eqExpr :: Eq Expr

derive instance eqElim :: Eq Elim
