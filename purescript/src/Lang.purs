module Lang where 

import Data.Tuple 
import Data.List (List, (:))

type Env = List (Tuple String Value)

data Typ = TypNum 
         | TypFunc Typ Typ 
         | TypList Typ 
         | TypPair Typ Typ 

data Value = ValNum Int 
           | ValPair Value Value 
           | ValNil 
           | ValCons Value Value 
           | ValClosure Env Elim
           | ValFailure String

type Var = String  

data Expr = ExprNum Int 
          | ExprVar Var
          | ExprPair Expr Expr 
          | ExprNil 
          | ExprCons Expr Expr
          | ExprLet Var Expr Expr 
          | ExprMatch Expr (List Elim)
          | ExprFunc Elim

data Elim = ElimVar Var Expr  
          | ElimNil Expr 
          | ElimCons Var Var Expr  
          | ElimPair Var Var Expr  
