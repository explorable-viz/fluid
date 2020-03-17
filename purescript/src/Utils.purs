module Utils where
  
import Data.List (List(..), (:), difference, union, singleton)
import Prelude ((<>))

import Expr

class FreeVars a where 
    freeVars :: a -> List Var

instance freeVarsExpr :: FreeVars Expr where
    freeVars (ExprNum i) = Nil
    freeVars (ExprVar x)  = singleton x
    freeVars (ExprLet x e1 e2)
        = union (freeVars e1) (difference (freeVars e2) (singleton x))
    freeVars (ExprAdd e1 e2)
        = union (freeVars e1) (freeVars e2)
    freeVars ExprTrue  = Nil
    freeVars ExprFalse = Nil
    freeVars (ExprPair e1 e2) = union (freeVars e1) (freeVars e2)
    freeVars (ExprPair_Fst e1) = freeVars e1 
    freeVars (ExprPair_Snd e2) = freeVars e2
    freeVars ExprNil = Nil
    freeVars (ExprCons e es) = (freeVars e <> freeVars es)
    freeVars (ExprCons_Head e) = freeVars e
    freeVars (ExprCons_Tail es) = freeVars es
    freeVars (ExprLet_Body e2) = freeVars e2
    freeVars (ExprMatch e elim) = union (freeVars e) (freeVars elim)
    freeVars (ExprApp e1 e2) = union (freeVars e1) (freeVars e2)
    freeVars (ExprApp_Fun e1) = freeVars e1
    freeVars (ExprFun elim) = freeVars elim

instance freeVarsElim :: FreeVars Elim where 
    freeVars (ElimVar x t e) = difference (freeVars e) (singleton x)
    freeVars (ElimPair x _ y _ e) = difference (freeVars e) (x:y:Nil)
    freeVars (ElimPair_Fst x _ e) = difference (freeVars e) (singleton x)
    freeVars (ElimPair_Snd y _ e) = difference (freeVars e) (singleton y)
    freeVars (ElimList bNil bCons) = union (freeVars bNil) (freeVars bCons)
    freeVars (ElimBool bTrue bFalse) = Nil

instance freeVarsBranchNil :: FreeVars BranchNil where 
    freeVars (BranchNil _ e) = freeVars e

instance freeVarsBranchCons :: FreeVars BranchCons where
    freeVars (BranchCons x xs _ e) = difference (freeVars e) (x:xs:Nil)
    freeVars (BranchCons_Head x _ e) = difference (freeVars e) (singleton x)
    freeVars (BranchCons_Tail xs _ e) = difference (freeVars e) (singleton xs)

instance freeVarsBranchTrue :: FreeVars BranchTrue where
    freeVars (BranchTrue e) = freeVars e 

instance freeVarsBranchFalse :: FreeVars BranchFalse where
    freeVars (BranchFalse e) = freeVars e 


