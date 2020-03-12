module Expr where 

import Data.Tuple 
import Data.List (List)
import Data.Maybe (Maybe)

type Env = List (Tuple Var Val)

type Ctx = List (Tuple Var Typ)

type Var = String  

data BranchNil = BranchNil Expr

data BranchCons = BranchCons Var Var Expr 

data BranchTrue = BranchTrue Expr 

data BranchFalse = BranchFalse Expr

data Typ = TypNum 
         | TypBool
         | TypFunc Typ Typ 
         | TypList Typ 
         | TypPair Typ Typ 

data Val = ValNum Int  
         | ValPair Val Val
         | ValNil 
         | ValCons Val Val
         | ValClosure Env Elim
         | ValFailure String

data Expr = ExprNum Int 
          | ExprVar Var
          | ExprBool Boolean
          | ExprPair Expr Expr 
          | ExprNil 
          | ExprCons Expr Expr
          | ExprLet Var Expr Expr 
          | ExprMatch Expr Elim
          | ExprFunc Elim
          | ExprApp Expr Expr

data Elim = ElimVar Var Expr  
          | ElimPair Var Var Expr  
          | ElimList BranchNil BranchCons  
          | ElimBool BranchTrue BranchFalse
