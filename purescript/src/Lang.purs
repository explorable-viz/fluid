module Lang where 

import Data.Tuple 
import Data.List (List, (:))
import Data.Maybe 

type Env = List (Tuple Var Val)

data Typ = TypNum 
         | TypFunc Typ Typ 
         | TypList Typ 
         | TypPair Typ Typ 

data Val = ValNum Int 
         | ValPair Val Val
         | ValNil 
         | ValCons Val Val
         | ValClosure Env Elim
         | ValFailure String

type Var = String  

data Expr = ExprNum Int 
          | ExprVar Var
          | ExprPair Expr Expr 
          | ExprNil 
          | ExprCons Expr Expr
          | ExprLet Var Expr Expr 
          | ExprMatch Expr Elim
          | ExprFunc Elim
          | ExprApp Expr Expr

data Elim = ElimVar Var Expr  
          | ElimNil Expr 
          | ElimList (Maybe (Tuple Var Var)) Expr  
          | ElimPair Var Var Expr  
