module Desugar where

import Prelude hiding (absurd)
import Data.List (List(..)) as L
import Data.List ((:), List)
import Data.Map (fromFoldable)
import Expr (RawExpr(..), Def(..)) as E
import Expr (Cont(..), Elim(..), Expr(..), Def, RecDefs, expr)
import Bindings (Var)
import DataType (Ctr(..))
import Primitive (concatMap, map) as P
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
   MatchAs SExpr Elim |
   IfElse SExpr SExpr SExpr |
   ListSeq Int Int |
   ListComp SExpr (List ListCompExpr) |
   Let Def SExpr |
   LetRec RecDefs SExpr

data ListCompExpr = Predicate SExpr | InputList SExpr SExpr

data SExpr = SExpr Boolean SugaredExpr


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
    = go e_rhs (numLists e_rhs)
    where
        numLists :: List ListCompExpr -> Int
        numLists L.Nil         = 0
        numLists (L.Cons e es) = case e of Predicate _   -> numLists es
                                           InputList _ _ -> numLists es + 1

        go :: List ListCompExpr -> Int -> Expr
        go L.Nil n = expr (E.Cons (desugar e_lhs) (expr E.Nil))
        go (e:es) n
            = case e of
                InputList bound_var list_expr ->
                    let Expr _ e'   = desugar bound_var
                        Expr _ es'  = desugar list_expr
                        σ           = bound_vars (expr e') (CExpr $ go es (n - 1))
                        ebody       = if n == 0 then (P.map σ $ expr es')
                                      else (P.concatMap σ $ expr es') :: Expr
                    in  expr $ E.Let (E.Def σ (expr e')) ebody

                Predicate p ->
                    let p' = desugar p
                        σ  = ElimConstr (fromFoldable [ (Ctr "True")  × CExpr (go es n)
                                                      , (Ctr "False") × CExpr (expr E.Nil)])
                    in  expr $ E.MatchAs p' σ
desugar (SExpr α (Var x))              = Expr α (E.Var x)
desugar (SExpr α (Op op))              = Expr α (E.Op op)
desugar (SExpr α (Str s))              = Expr α (E.Str s)
desugar (SExpr α (Constr ctr args))    = Expr α (E.Constr ctr (map desugar args))
desugar (SExpr α (True))               = Expr α E.True
desugar (SExpr α (False))              = Expr α E.False
desugar (SExpr α (Pair e1 e2))         = Expr α (E.Pair (desugar e1) (desugar e2))
desugar (SExpr α (Nil))                = Expr α E.Nil
desugar (SExpr α (Cons e es))          = Expr α (E.Cons (desugar e) (desugar es))
desugar (SExpr α (Lambda σ))           = Expr α (E.Lambda σ)
desugar (SExpr α (App e1 e2))          = Expr α (E.App (desugar e1) (desugar e2))
desugar (SExpr α (BinaryApp e1 op e2)) = Expr α (E.BinaryApp (desugar e1) op (desugar e2))
desugar (SExpr α (MatchAs e σ))        = Expr α (E.MatchAs (desugar e) σ)
desugar (SExpr α (Let def e))          = Expr α (E.Let def (desugar e))
desugar (SExpr α (LetRec δ e))         = Expr α (E.LetRec δ (desugar e))
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