module Project where

import Prelude ((==))
import Expr

class Projectable a where
    project :: a -> a

instance projExpr :: Projectable Expr where
    project ExprBottom            = ExprBottom
    project (ExprVar x)           = ExprVar x
    project ExprTrue              = ExprTrue
    project ExprFalse             = ExprFalse
    project (ExprInt n)           = ExprInt n
    project (ExprPair e1 e2)      = ExprPair (project e1) (project e2)
    project (ExprPairFst e1 )     = project e1
    project (ExprPairSnd e2 )     = project e2
    project (ExprNil)             = ExprNil
    project (ExprCons e es)       = ExprCons (project e) (project es)
    project (ExprConsHead e)      = project e
    project (ExprConsTail es)     = project es
    project (ExprLetrec fun σ e)  = ExprLetrec fun (project σ) (project e)
    project (ExprApp e1 e2)       = ExprApp (project e1) (project e2)
    project (ExprLet x e1 e2)     = ExprLet x (project e1) (project e2)
    project (ExprLetBody x e1 e2) = project e2
    project (ExprMatch e σ)       = ExprMatch (project e) σ
    project (ExprAdd e1 e2)       = ExprAdd e1 e2

instance projElim :: Projectable Elim where
      project (ElimVar { x, tx, e })   = ElimVar { x, tx, e: project e }
      project (ElimPair { x, tx, y, ty, e })   = ElimPair { x, tx, y, ty, e: project e }
      project (ElimList { bnil: e, bcons: { x, y, e: e' } })   = ElimList { bnil: project e, bcons: { x, y, e: project e' } }
      project (ElimBool { btrue: e, bfalse: e' })  = ElimBool { btrue: project e, bfalse: project e' }

instance projVal :: Projectable Val where
      project ValBottom          = ValBottom
      project ValTrue            = ValTrue
      project ValFalse           = ValFalse
      project (ValInt n)         = ValInt n
      project (ValPair x y)      = ValPair (project x) (project y)
      project (ValPairFst x)     = project x
      project (ValPairSnd y)     = project y
      project (ValClosure ρ f σ) = (ValClosure (project ρ) f (project σ))
      project (ValNil)           = ValNil
      project (ValCons x xs)     = ValCons (project x) (project xs)
      project (ValConsHead x)    = project x
      project (ValConsTail xs)   = project xs
      project (ValFailure x)     = ValFailure x

instance projEnv :: Projectable (Bindings Val) where
      project Empty = Empty
      project (bs :+: Bind x v) =  if   v == ValBottom
                                   then project bs :+: Bind x v
                                   else project bs

instance projCtx :: Projectable (Bindings Typ) where
      project Empty = Empty
      project (bs :+: Bind x t) =  if   t == TypBottom
                                   then project bs :+: Bind x t
                                   else project bs

instance projTyp :: Projectable Typ where
      project TypBottom       = TypBottom
      project TypBool         = TypBool
      project TypInt          = TypInt
      project (TypPair a b)   = TypPair (project a) (project b)
      project (TypPairFst a)  = project a
      project (TypPairSnd b)  = project b
      project (TypList a)     = TypList (project a)
      project (TypListHead a) = project a
      project (TypListTail a) = TypList (project a)
      project (TypFun a b)    = TypFun (project a) (project b)
      project (TypFailure x)  = TypFailure x