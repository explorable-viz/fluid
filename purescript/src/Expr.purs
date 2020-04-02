module Expr where

import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))
import Data.Eq (class Eq)
import Data.Show

type Φ = String

type Var = String

data T2 a b = T2 a b

derive instance eqT2 :: (Eq a, Eq b) => Eq (T2 a b)
instance showT2 :: (Show a, Show b) => Show (T2 a b) where
  show (T2 a b) = "T2 " <> show a <> " " <> show b

data T3 a b c = T3 a b c

derive instance eqT3 :: (Eq a, Eq b, Eq c) => Eq (T3 a b c)
instance showT3 :: (Show a, Show b, Show c) => Show (T3 a b c) where
  show (T3 a b c) = "T3 " <> show a <> " " <> show b <> " " <> show c

data Env = EnvNil | EnvSnoc Env (T2 Var Val)

derive instance eqEnv :: Eq Env
instance showEnv :: Show Env where 
  show EnvNil = "EnvNil"
  show (EnvSnoc env (T2 x v)) = show env <> ":∈: (" <> show x <> ", " <> show v <> ")"

appendToEnv :: Env -> (T2 Var Val) -> Env
appendToEnv env x = EnvSnoc env x

infixl 5 appendToEnv as :∈:

concEnv :: Env -> Env -> Env 
concEnv env1 EnvNil = env1
concEnv env1 (EnvSnoc vs v) = EnvSnoc (concEnv env1 vs) v

findVarVal :: Var -> Env -> Maybe Val
findVarVal _ EnvNil = Nothing
findVarVal x (EnvSnoc vs (T2 var val)) = if x == var then Just val else findVarVal x vs

data Ctx = CtxNil | CtxSnoc Ctx (T2 Var Typ)

derive instance eqCtx :: Eq Ctx
instance showCtx :: Show Ctx where 
  show CtxNil = "EnvNil"
  show (CtxSnoc ctx (T2 x v)) = show ctx <> ":∁: (" <> show x <> ", " <> show v <> ")"

appendToCtx :: Ctx -> (T2 Var Typ) -> Ctx
appendToCtx ctx x = CtxSnoc ctx x

infixl 5 appendToCtx as :∁:

concCtx :: Ctx -> Ctx -> Ctx 
concCtx ctx1 CtxNil = ctx1
concCtx ctx1 (CtxSnoc cs c) = CtxSnoc (concCtx ctx1 cs) c

findVarTyp :: Var -> Ctx -> Maybe Typ
findVarTyp _ CtxNil = Nothing
findVarTyp x (CtxSnoc cs (T2 var typ)) = if x == var then Just typ else findVarTyp x cs
  
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

data Match = MatchVar Var 
           | MatchTrue
           | MatchFalse
           | MatchPair Var Var
           | MatchNil
           | MatchCons Var Var

derive instance eqMatch :: Eq Match
instance showMatch :: Show Match where 
  show (MatchVar v )     = "MatchVar " <> v
  show (MatchPair t1 t2) = "MatchPair " <> show t1 <> show t2
  show (MatchNil)        = "MatchNil "
  show (MatchCons x xs)  = "MatchCons " <> x <> " " <> xs
  show (MatchTrue)       = "MatchTrue"
  show (MatchFalse)      = "MatchFalse"

data Trace = TraceBottom
           | TraceVar Var 
           | TraceNum Int 
           | TracePair Trace Trace
           | TraceNil
           | TraceCons Trace Trace
           | TraceApp Trace Trace Match Trace
           | TraceMatch Trace Match Trace
           | TraceAdd Trace Trace 
           | TraceLet Var Trace Trace
           | TraceTrue
           | TraceFalse

derive instance eqTrace :: Eq Trace
instance showTrace :: Show Trace where 
  show (TraceBottom)        = "TraceBottom"
  show (TraceVar v )        = "TraceVar " <> v
  show (TraceNum n)         = "TraceNum " <> show n
  show (TracePair t1 t2)    = "TracePair " <> show t1 <> show t2
  show (TraceNil)           = "TraceNil "
  show (TraceCons x xs)     = "TraceCons " <> show x <> " " <> show xs
  show (TraceApp t1 t2 m b) = "TraceCons " <> show t1 <> " " <> show t2 <> " " <> show m <> " " <> show b
  show (TraceLet x e1 e2)   = "TraceLet " <> x <> " " <> show e1 <> " " <> show e1
  show (TraceAdd t1 t2)     = "TraceAdd " <> show t1 <> " " <> show t2
  show (TraceMatch t1 m t2) = "TraceMatch " <> show t1 <> " " <> show m <> " " <> show t2
  show TraceTrue            = "TraceTrue "
  show TraceFalse           = "TraceFalse "