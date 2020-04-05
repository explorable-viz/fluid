module Expr where

import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))
import Data.Eq (class Eq)
import Data.Show

type Φ = String

type Var = String

data Bind a = Bind Var a

derive instance eqBind :: (Eq a) => Eq (Bind a)
instance showBind :: (Show a) => Show (Bind a) where
  show (Bind x a) = "Bind " <> show x <> " " <> show a

data T3 a b c = T3 a b c

derive instance eqT3 :: (Eq a, Eq b, Eq c) => Eq (T3 a b c)
instance showT3 :: (Show a, Show b, Show c) => Show (T3 a b c) where
  show (T3 a b c) = "T3 " <> show a <> " " <> show b <> " " <> show c

data Bindings a =
  Empty | Snoc (Bindings a) (Bind a)

derive instance eqBindings :: (Eq a) => Eq (Bindings a)
instance showBindings :: (Show a) => Show (Bindings a) where
  show Empty = "Empty"
  show (Snoc m (Bind k v)) = show m <> ":+: (" <> show k <> ", " <> show v <> ")"

type Env = Bindings Val

infixl 5 Snoc as :∈:

conc :: forall a . Bindings a -> Bindings a -> Bindings a
conc m Empty = m
conc m1 (Snoc m2 kv) = Snoc (conc m1 m2) kv

find :: forall a . Var -> Bindings a -> Maybe a
find _ Empty = Nothing
find x (Snoc m (Bind k v)) = if x == k then Just v else find x m

type Ctx = Bindings Typ

data BranchNil -- (type of list, branch)
                = BranchNil Typ Expr

derive instance eqBranchNil :: Eq BranchNil
instance showBranchNil :: Show BranchNil where
  show (BranchNil t e) = "BranchNil " <> show t <> " " <> show e

data BranchCons -- (x, xs, type(x), branch)
                = BranchCons Var Var Typ Expr


derive instance eqBranchCons :: Eq BranchCons
instance showBranchCons :: Show BranchCons where
  show (BranchCons x xs t e) = "BranchCons " <> show x <> " " <> show xs <> " " <> show t <> " " <> show e

data BranchTrue = BranchTrue Expr

derive instance eqBranchTrue :: Eq BranchTrue
instance showBranchTrue :: Show BranchTrue where
  show (BranchTrue e) = "BranchTrue " <> show e

data BranchFalse = BranchFalse Expr

derive instance eqBranchFalse :: Eq BranchFalse
instance showBranchFalse :: Show BranchFalse where
  show (BranchFalse e) = "BranchFalse " <> show e

data Availability = Top | Bottom

derive instance eqAvailability :: Eq Availability
instance showAvailability :: Show Availability where
  show Top    = "Top"
  show Bottom = "Bottom"


data Typ = TypBottom
         | TypNum
         | TypBool
         | TypFun Typ Typ
         | TypList Typ
         | TypPair Typ Typ
         | TypFailure String

derive instance eqTyp :: Eq Typ
instance showTyp :: Show Typ where
  show TypBottom       = "TypBottom"
  show TypNum          = "TypNum"
  show TypBool         = "TypBool"
  show (TypFun t1 t2)  = "TypFun " <> show t1 <> " " <> show t2
  show (TypList t)     = "TypList " <> show t
  show (TypPair t1 t2) = "TypPair " <> show t1 <> " " <> show t2
  show (TypFailure s)  = "TypFailure " <> s


data Val = ValBottom
         | ValTrue
         | ValFalse
         | ValNum Int
         | ValClosure Env String Elim
         | ValPair Val Val | ValPair_Del Val Val
         | ValNil
         | ValCons Val Val | ValCons_Del Val Val
         | ValFailure String

derive instance eqVal :: Eq Val
instance showVal :: Show Val where
  show ValBottom                 = "⊥"
  show ValTrue                   = "ValTrue"
  show ValFalse                  = "ValFalse"
  show (ValNum n)                = "ValNum " <> show n
  show (ValPair x y)             = "ValPair " <> show x <> " " <> show y
  show (ValPair_Del x y)         = "ValPair_Del " <> show x <> " " <> show y
  show ValNil                    = "ValNil"
  show (ValCons x xs)            = "ValCons " <> show x <> " " <> show xs
  show (ValCons_Del x xs)        = "ValCons_Del " <> show x <> " " <> show xs
  show (ValClosure env fun elim) = "ValClosure " <> show env <> " " <> show fun <> " " <> show elim
  show (ValFailure s)            = "ValFailure " <> s


data Expr = ExprBottom
          | ExprNum Int
          | ExprVar Var
          | ExprTrue
          | ExprFalse
          | ExprPair Expr Expr | ExprPair_Del Expr Expr
          | ExprNil
          | ExprCons Expr Expr | ExprCons_Del Expr Expr
          | ExprLet Var Expr Expr | ExprLet_Body Var Expr Expr
          | ExprMatch Expr Elim
          | ExprLetrec String Elim Expr
          | ExprApp Expr Expr
          | ExprAdd Expr Expr

derive instance eqExpr :: Eq Expr
instance showExpr :: Show Expr where
  show ExprBottom              = "ExprBottom"
  show (ExprNum n)             = "ExprNum " <> show n
  show (ExprVar v)             = "ExprVar " <> show v
  show ExprTrue                = "ExprTrue"
  show ExprFalse               = "ExprFalse"
  show (ExprPair x y)          = "ExprPair " <> show x <> " " <> show y
  show (ExprPair_Del x y)      = "ExprPair_Del " <> show x <> " " <> show y
  show ExprNil                 = "ExprNil"
  show (ExprCons x xs)         = "ExprCons " <> show x <> " " <> show xs
  show (ExprCons_Del x xs)     = "ExprCons_Del " <> show x <> " " <> show xs
  show (ExprLet v e1 e2)       = "ExprLet " <> show v <> " " <> show e1 <> " " <> show e2
  show (ExprLet_Body v e1 e2)  = "ExprLet_Body " <> show v <> " " <> show e1 <> " " <> show e2
  show (ExprMatch e elim)      = "ExprMatch " <> show e <> " " <> show elim
  show (ExprLetrec fun elim e) = "ExprLetrec " <> show fun <> " " <> show elim <> " " <> show e
  show (ExprApp e1 e2)         = "ExprApp " <> show e1 <> " " <> show e2
  show (ExprAdd e1 e2)         = "ExprAdd " <> show e1 <> " " <> show e2


data Elim = -- (x, type(x), branch)
            ElimVar Var Typ Expr
            -- (x, type(x), y, type(y), branch)
          | ElimPair Var Typ Var Typ Expr
          | ElimList BranchNil BranchCons
          | ElimBool BranchTrue BranchFalse

derive instance eqElim :: Eq Elim
instance showElim :: Show Elim where
  show (ElimVar v t e)          = "ElimVar " <> v <> ":" <> show t <> " " <> show e
  show (ElimPair v1 t1 v2 t2 e) = "ElimPair " <> show v1 <> ":" <> show t1 <> " " <> show v2 <> ":" <> show t2 <> show e
  show (ElimList bnil bcons)    = "ElimList " <> show bnil <> " " <> show bcons
  show (ElimBool btrue bfalse)  = "ElimBool " <> show btrue <> " " <> show bfalse
