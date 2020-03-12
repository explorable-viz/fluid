module Expr where

import Prelude ((==))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Eq 

type Var = String  

data Env = EnvNil | EnvSnoc Env (Tuple Var Val)

derive instance eqEnv :: Eq Env

appendToEnv :: Env -> (Tuple Var Val) -> Env
appendToEnv env x = EnvSnoc env x

concEnv :: Env -> Env -> Env 
concEnv env1 EnvNil = env1
concEnv env1 (EnvSnoc vs v) = EnvSnoc (concEnv env1 vs) v

findVarVal :: Var -> Env -> Maybe Val
findVarVal _ EnvNil = Nothing
findVarVal x (EnvSnoc vs (Tuple var val)) = if x == var then Just val else findVarVal x vs 


data Ctx = CtxNil | CtxSnoc Ctx (Tuple Var Typ)

derive instance eqCtx :: Eq Ctx

appendToCtx :: Ctx -> (Tuple Var Typ) -> Ctx 
appendToCtx ctx x = CtxSnoc ctx x

concCtx :: Ctx -> Ctx -> Ctx 
concCtx ctx1 CtxNil = ctx1
concCtx ctx1 (CtxSnoc cs c) = CtxSnoc (concCtx ctx1 cs) c

findVarTyp :: Var -> Ctx -> Maybe Typ 
findVarTyp _ CtxNil = Nothing 
findVarTyp x (CtxSnoc cs (Tuple var typ)) = if x == var then Just typ else findVarTyp x cs 


data BranchNil = BranchNil Expr

derive instance eqBranchNil :: Eq BranchNil

data BranchCons = BranchCons Var Var Expr 

derive instance eqBranchCons :: Eq BranchCons

data BranchTrue = BranchTrue Expr 

derive instance eqBranchTrue :: Eq BranchTrue

data BranchFalse = BranchFalse Expr

derive instance eqBranchFalse :: Eq BranchFalse

data Typ = TypNum 
         | TypBool
         | TypFunc Typ Typ
         | TypList Typ
         | TypPair Typ Typ

derive instance eqTyp :: Eq Typ

data Val = ValTrue
         | ValFalse
         | ValNum Int  
         | ValPair Val Val
         | ValNil
         | ValCons Val Val
         | ValClosure Env Elim
         | ValFailure String

derive instance eqVal :: Eq Val

data Expr = ExprNum Int 
          | ExprVar Var
          | ExprTrue
          | ExprFalse
          | ExprPair Expr Expr 
          | ExprNil 
          | ExprCons Expr Expr
          | ExprLet Var Expr Expr
          | ExprMatch Expr Elim
          | ExprFunc Elim
          | ExprApp Expr Expr
          | ExprAdd Expr Expr

derive instance eqExpr :: Eq Expr

data Elim = ElimVar Var Expr
          | ElimPair Var Var Expr
          | ElimList BranchNil BranchCons
          | ElimBool BranchTrue BranchFalse

derive instance eqElim :: Eq Elim