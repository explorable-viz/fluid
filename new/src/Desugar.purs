module Desugar where

import Prelude hiding (absurd)
import Data.List (List(..)) as L
import Data.List ((:), List)
import Data.Map (fromFoldable, Map)
import Expr (RawExpr(..)) as E
import Expr (Cont(..), Elim(..), Expr(..), RawExpr, Def, RecDefs, RecDef(..), expr)
import Bindings (Var)
import DataType (Ctr(..))
import Util ((×), absurd, error)

data SugaredExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List SExpr) |
   True | False |
   Pair SExpr SExpr |
   Nil | Cons SExpr SExpr |
   Lambda Elim |
   App SExpr SExpr |
   BinaryApp SExpr Var SExpr |
   MatchAs SExpr SElim |
   IfElse SExpr SExpr SExpr |
   ListSeq Int Int |
   ListComp SExpr (List ListCompExpr) |
   Let Def SExpr |
   LetRec RecDefs SExpr

data ListCompExpr = Predicate SExpr | InputList SExpr SExpr

data SExpr = SExpr Boolean SugaredExpr

data SCont = SCNone | SCExpr SExpr | SCElim SElim

data SElim =
   SElimVar Var SCont |
   SElimConstr (Map Ctr SCont)

desugar :: SExpr -> Expr
desugar (SExpr α (IfElse e1 e2 e3))
    = let e1' = desugar e1
          e2' = desugar e2
          e3' = desugar e3
          σ = ElimConstr (fromFoldable [ (Ctr "True")  × CExpr e2'
                                       , (Ctr "False") × CExpr e3'])
      in  Expr α (E.MatchAs e1' σ)
desugar (SExpr α (ListSeq a z))
    | a <= z    = Expr α (go z E.Nil)
    where go n acc = let acc' = E.Cons (Expr false (E.Int n)) (Expr false acc)
                     in  if n == a then acc' else go (n - 1) acc'
    | otherwise = error absurd
desugar (SExpr α (ListComp e_lhs e_rhs))
  = let go (e:es)
            = case e of
                InputList bound_var list_expr ->
                    let Expr _ e'   = desugar bound_var
                        σ           = bound_vars (Expr α e')
                        Expr _ es'  = desugar list_expr
                    in  expr $ E.LetRec (RecDef "f" (σ $ CExpr $ go es):L.Nil) (expr $ E.App (expr $ E.Var "f") (expr $ es'))
                Predicate p ->
                    let p' = desugar p
                        σ  = ElimConstr (fromFoldable [ (Ctr "True")  × CExpr (go es)
                                                      , (Ctr "False") × CExpr (expr E.Nil)])
                    in  expr $ E.MatchAs p' σ
        go L.Nil
            = expr (E.Cons (desugar e_lhs) (expr E.Nil)) -- concatMap 
    in  go e_rhs

desugar _ = error absurd

bound_vars :: Expr -> Cont -> Elim
bound_vars (Expr _ (E.Var x)) κ
    = ElimVar x κ
bound_vars (Expr _ (E.Pair x y)) κ
    = ElimConstr (fromFoldable [(Ctr "Pair") × (CElim $ bound_vars x (CElim $ bound_vars y κ))])
bound_vars (Expr _ (E.Nil)) κ
    = ElimConstr (fromFoldable [(Ctr "Nil") × κ])
bound_vars (Expr _ (E.Cons x xs)) κ
    = ElimConstr (fromFoldable [(Ctr "Cons") × (CElim $ bound_vars x (CElim $ bound_vars xs κ))])
bound_vars (Expr _ (E.True)) κ
    = ElimConstr (fromFoldable [(Ctr "True") × κ])
bound_vars (Expr _ (E.False)) κ
    = ElimConstr (fromFoldable [(Ctr "False") × κ])
bound_vars _ _ = error absurd