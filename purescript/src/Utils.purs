module Utils where

import Data.List (List(..), (:), difference, union, singleton)
import Prelude ((<>))

import Expr

class FreeVars a where
    freeVars :: a -> List Var

instance freeVarsExpr :: FreeVars Expr where
    freeVars (ExprInt i)             = Nil
    freeVars (ExprVar x)             = singleton x
    freeVars (ExprLet x e1 e2)       = union (freeVars e1) (difference (freeVars e2) (singleton x))
    freeVars (ExprAdd e1 e2)         = union (freeVars e1) (freeVars e2)
    freeVars ExprTrue                = Nil
    freeVars ExprFalse               = Nil
    freeVars (ExprPair e1 e2)        = union (freeVars e1) (freeVars e2)
    freeVars (ExprPairSnd e2)        = (freeVars e2)
    freeVars (ExprPairFst e1)        = (freeVars e1)
    freeVars ExprNil                 = Nil
    freeVars (ExprCons e es)         = (freeVars e <> freeVars es)
    freeVars (ExprConsHead e )       = freeVars e
    freeVars (ExprConsTail es)       = freeVars es
    freeVars (ExprLetBody x e1 e2)   = freeVars e2
    freeVars (ExprMatch e elim)      = union (freeVars e) (freeVars elim)
    freeVars (ExprApp e1 e2)         = union (freeVars e1) (freeVars e2)
    freeVars (ExprLetrec fun elim e) = union (freeVars elim) (difference (freeVars e) (singleton fun))
    freeVars ExprBottom              = Nil

instance freeVarsElim :: FreeVars Elim where
    freeVars (ElimVar { x, e })  = difference (freeVars e) (singleton x)
    freeVars (ElimPair { x, y, e })  = difference (freeVars e) (x:y:Nil)
    freeVars (ElimList { bnil: e, bcons: { e: e' } })   = union (freeVars e) (freeVars e')
    freeVars (ElimBool { btrue: e, bfalse: e' }) = Nil
