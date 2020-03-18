module Expr where

import Prelude ((==), (<>))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Eq (class Eq)
import Data.Show

type Var = String

data Env = EnvNil | EnvSnoc Env (Tuple Var Val)

derive instance eqEnv :: Eq Env
instance showEnv :: Show Env where 
  show EnvNil = "EnvNil"
  show (EnvSnoc env (Tuple x v)) = show env <> ":< (" <> show x <> ", " <> show v <> ")"

appendToEnv :: Env -> (Tuple Var Val) -> Env
appendToEnv env x = EnvSnoc env x

infixl 5 appendToEnv as :<

concEnv :: Env -> Env -> Env 
concEnv env1 EnvNil = env1
concEnv env1 (EnvSnoc vs v) = EnvSnoc (concEnv env1 vs) v

findVarVal :: Var -> Env -> Maybe Val
findVarVal _ EnvNil = Nothing
findVarVal x (EnvSnoc vs (Tuple var val)) = if x == var then Just val else findVarVal x vs

data Ctx = CtxNil | CtxSnoc Ctx (Tuple Var Typ)

derive instance eqCtx :: Eq Ctx
instance showCtx :: Show Ctx where 
  show CtxNil = "EnvNil"
  show (CtxSnoc ctx (Tuple x v)) = show ctx <> ":> (" <> show x <> ", " <> show v <> ")"

appendToCtx :: Ctx -> (Tuple Var Typ) -> Ctx
appendToCtx ctx x = CtxSnoc ctx x

infixl 5 appendToCtx as :>

concCtx :: Ctx -> Ctx -> Ctx 
concCtx ctx1 CtxNil = ctx1
concCtx ctx1 (CtxSnoc cs c) = CtxSnoc (concCtx ctx1 cs) c

findVarTyp :: Var -> Ctx -> Maybe Typ
findVarTyp _ CtxNil = Nothing
findVarTyp x (CtxSnoc cs (Tuple var typ)) = if x == var then Just typ else findVarTyp x cs
  
data BranchNil -- (type of list, branch)
                = BranchNil Typ Expr

derive instance eqBranchNil :: Eq BranchNil
instance showBranchNil :: Show BranchNil where
  show (BranchNil t e) = "BranchNil " <> show t <> " " <> show e

data BranchCons -- (x, xs, type(x), branch)
                = BranchCons Var Var Typ Expr 
                -- (x, type(x), branch)
                | BranchCons_Head Var Typ Expr 
                -- ((x:xs), type(x), branch)
                | BranchCons_Tail Var Typ Expr

derive instance eqBranchCons :: Eq BranchCons
instance showBranchCons :: Show BranchCons where
  show (BranchCons x xs t e) = "BranchCons " <> show x <> " " <> show xs <> " " <> show t <> " " <> show e
  show (BranchCons_Head x t e) = "BranchCons_Head " <> show x <> " " <> show t <> " " <> show e
  show (BranchCons_Tail xs t e) = "BranchCons_Tail "  <> show xs <> " " <> show t <> " " <> show e

data BranchTrue = BranchTrue Expr

derive instance eqBranchTrue :: Eq BranchTrue
instance showBranchTrue :: Show BranchTrue where 
  show (BranchTrue e) = "BranchTrue " <> show e

data BranchFalse = BranchFalse Expr

derive instance eqBranchFalse :: Eq BranchFalse
instance showBranchFalse :: Show BranchFalse where 
  show (BranchFalse e) = "BranchFalse " <> show e

data Typ = TypNum
         | TypBool
         | TypFun Typ Typ
         | TypList Typ | TypList_Head Typ | TypList_Tail Typ
         | TypPair Typ Typ | TypPair_Fst Typ | TypPair_Snd Typ
         | TypFailure String

derive instance eqTyp :: Eq Typ
instance showTyp :: Show Typ where 
  show TypNum = "TypNum"
  show TypBool = "TypBool"
  show (TypFun t1 t2) = "TypFun " <> show t1 <> " " <> show t2
  show (TypList t) = "TypList " <> show t
  show (TypList_Head t) = "TypList_Head " <> show t
  show (TypList_Tail t) = "TypList_Tail " <> show t
  show (TypPair t1 t2) = "TypPair " <> show t1 <> " " <> show t2 
  show (TypPair_Fst t1) = "TypPair_Fst " <> show t1
  show (TypPair_Snd t2) = "TypPair_Snd " <> show t2
  show (TypFailure s) = "TypFailure " <> s


data Val = ValTrue
         | ValFalse
         | ValNum Int
         | ValPair Val Val | ValPair_Fst Val | ValPair_Snd Val
         | ValNil
         | ValCons Val Val | ValCons_Head Val | ValCons_Tail Val
         | ValClosure Env Elim
         | ValFailure String

derive instance eqVal :: Eq Val
instance showVal :: Show Val where 
  show ValTrue = "ValTrue"
  show ValFalse = "ValFalse"       
  show (ValNum n) = "ValNum " <> show n 
  show (ValPair x y) = "ValPair " <> show x <> " " <> show y
  show (ValPair_Fst x) = "ValPair_Fst " <> show x
  show (ValPair_Snd y) = "ValPair_Snd " <> show y
  show ValNil = "ValNil"
  show (ValCons x xs) = "ValCons " <> show x <> " " <> show xs 
  show (ValCons_Head x)  = "ValCons_Head " <> show x 
  show (ValCons_Tail xs) = "ValCons_Tail " <> show xs
  show (ValClosure env elim) = "ValClosure " <> show env <> " " <> show elim
  show (ValFailure s) = "ValFailure " <> s


data Expr = ExprNum Int
          | ExprVar Var
          | ExprTrue
          | ExprFalse
          | ExprPair Expr Expr | ExprPair_Fst Expr | ExprPair_Snd Expr
          | ExprNil
          | ExprCons Expr Expr | ExprCons_Head Expr | ExprCons_Tail Expr
          | ExprLet Var Expr Expr | ExprLet_Body Expr
          | ExprMatch Expr Elim
          | ExprFun Elim
          | ExprApp Expr Expr | ExprApp_Fun Expr
          | ExprAdd Expr Expr

derive instance eqExpr :: Eq Expr
instance showExpr :: Show Expr where
  show (ExprNum n) = "ExprNum " <> show n
  show (ExprVar v) = "ExprVar " <> show v
  show ExprTrue  = "ExprTrue"
  show ExprFalse = "ExprFalse"
  show (ExprPair x y) = "ExprPair " <> show x <> " " <> show y 
  show (ExprPair_Fst x) = "ExprPair_Fst " <> show x 
  show (ExprPair_Snd y) = "ExprPair_Snd " <> show y
  show ExprNil = "ExprNil"
  show (ExprCons x xs) = "ExprCons " <> show x <> " " <> show xs
  show (ExprCons_Head x) = "ExprCons_Head " <> show x
  show (ExprCons_Tail xs) = "ExprCons_Tail " <> show xs 
  show (ExprLet v e1 e2) = "ExprLet " <> show v <> " " <> show e1 <> " " <> show e2
  show (ExprLet_Body e) = "ExprLet_Body " <> show e
  show (ExprMatch e elim) = "ExprMatch " <> show e <> " " <> show elim
  show (ExprFun elim) = "ExprFun " <> show elim
  show (ExprApp e1 e2) = "ExprApp " <> show e1 <> " " <> show e2
  show (ExprApp_Fun e1) = "ExprApp_Fun " <> show e1
  show (ExprAdd e1 e2) = "ExprAdd " <> show e1 <> " " <> show e2

            
data Elim = -- (x, type(x), branch)
            ElimVar Var Typ Expr
            -- (x, type(x), y, type(y), branch)
          | ElimPair Var Typ Var Typ Expr | ElimPair_Fst Var Typ Expr | ElimPair_Snd Var Typ Expr
          | ElimList BranchNil BranchCons 
          | ElimBool BranchTrue BranchFalse

derive instance eqElim :: Eq Elim
instance showElim :: Show Elim where 
  show (ElimVar v t e) = "ElimVar " <> v <> ":" <> show t <> " " <> show e
  show (ElimPair v1 t1 v2 t2 e) = "ElimPair " <> show v1 <> ":" <> show t1 <> " " <> show v2 <> ":" <> show t2 <> show e
  show (ElimPair_Fst v t e) = "ElimPair_Fst " <> show v <> ":" <> show t <> " " <> show e
  show (ElimPair_Snd v t e) = "ElimPair_Snd " <> show v <> ":" <> show t <> " " <> show e
  show (ElimList bnil bcons) = "ElimList " <> show bnil <> " " <> show bcons 
  show (ElimBool btrue bfalse) = "ElimBool " <> show btrue <> " " <> show bfalse