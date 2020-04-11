module Project where

import Expr
import Availability

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

instance projBranchNil :: Projectable BranchNil where
      project (BranchNil e)  = BranchNil (project e)

instance projBranchCons :: Projectable BranchCons where
      project (BranchCons x xs  e) = BranchCons x xs  (project e)

instance projBranchTrue :: Projectable BranchTrue where
      project (BranchTrue e) = BranchTrue (project e)

instance projBranchFalse :: Projectable BranchFalse where
      project (BranchFalse e)  = BranchFalse (project e)


instance projElim :: Projectable Elim where
      project (ElimVar x tx e)         = ElimVar x tx (project e)
      project (ElimPair x tx y ty e)   = ElimPair x tx y ty (project e)
      project (ElimList bNil bCons )   = ElimList (project bNil) (project bCons)
      project (ElimBool bTrue bFalse)  = ElimBool (project bTrue) (project bFalse)

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

instance projSnoc :: (Projectable a, Available a) => Projectable (Bindings a) where 
      project Empty = Empty
      project (bs :+: Bind x a) =  if   isTop a  
                                   then project bs :+: Bind x a
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