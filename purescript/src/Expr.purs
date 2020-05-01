module Expr where

import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))
import Data.Eq (class Eq)
import Data.Show

data T3 a b c = T3 a b c

type Var = String

data Bind a = Bind Var a

data Bindings a =
  Empty | Snoc (Bindings a) (Bind a)

infixl 5 Snoc as :+:

conc :: forall a . Bindings a -> Bindings a -> Bindings a
conc m Empty = m
conc m1 (Snoc m2 kv) = Snoc (conc m1 m2) kv

infixl 5 conc as :++:

find :: forall a . Var -> Bindings a -> Maybe a
find _ Empty = Nothing
find x (Snoc m (Bind k v)) = if x == k then Just v else find x m

type Ctx = Bindings Typ

type Env = Bindings Val

data Availability = Top | Bottom

data Typ = TypBottom
         | TypInt
         | TypBool
         | TypFun Typ Typ
         | TypList Typ
         | TypPair Typ Typ
         | TypFailure String

data Val = ValBottom
         | ValTrue | ValTrueSel
         | ValFalse | ValFalseSel
         | ValInt Int | ValIntSel Int
         | ValClosure Env String Elim
         | ValPair Val Val | ValPairSel Val Val
         | ValNil | ValNilSel
         | ValCons Val Val | ValConsSel Val Val
         | ValFailure String

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

derive instance eqBind :: (Eq a) => Eq (Bind a)
instance showBind :: (Show a) => Show (Bind a) where
  show (Bind x a) = "Bind " <> show x <> " " <> show a

derive instance eqBindings :: (Eq a) => Eq (Bindings a)
instance showBindings :: (Show a) => Show (Bindings a) where
  show Empty = "Empty"
  show (Snoc m (Bind k v)) = show m <> ":+: (" <> show k <> ", " <> show v <> ")"

derive instance eqAvailability :: Eq Availability
instance showAvailability :: Show Availability where
  show Top    = "Top"
  show Bottom = "Bottom"

derive instance eqTyp :: Eq Typ

derive instance eqVal :: Eq Val

derive instance eqExpr :: Eq Expr

derive instance eqElim :: Eq Elim
