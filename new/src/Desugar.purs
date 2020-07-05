module Desugar where

import Prelude hiding (absurd)
import Data.Foldable (foldl)
import Data.List ((:), List)
import Data.List (List(..)) as L
import Data.Map (fromFoldable, empty) as M
import DataType (Ctr, cCons, cNil, cTrue, cFalse)
import Expr (Cont, Cont'(..), Elim, Elim'(..), Expr, Expr'(..), RecDefs, Var, VarDef, concatMapE, mapE, expr)
import Expr (RawExpr'(..), VarDef'(..)) as E
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
   Let VarDef SExpr |
   LetRec RecDefs SExpr

data ListCompExpr = Predicate SExpr | InputList SExpr SExpr

data SExpr = SExpr Boolean SugaredExpr

desugar :: SExpr -> Expr
desugar (SExpr α (IfElse e1 e2 e3))
    = let e1' = desugar e1
          e2' = desugar e2
          e3' = desugar e3
          σ = ElimConstr (M.fromFoldable [ cTrue × Body e2'
                                         , cFalse × Body e3'])
      in  Expr α (E.MatchAs e1' σ)
desugar (SExpr α (ListSeq a z))
    | a <= z    = Expr α (go z (E.Constr cNil L.Nil))
    where go n acc = let acc' = E.Constr cCons ((expr $ E.Int n):(expr $ acc):L.Nil)
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
        go (e:es) n
            = case e of
                InputList bound_var list_expr ->
                    let Expr _ e'   = desugar bound_var
                        Expr _ es'  = desugar list_expr
                        σ           = bound_vars (expr e') (Body $ go es (n - 1))
                        ebody       = if n == 0 then mapE σ $ expr es'
                                      else concatMapE σ $ expr es' :: Expr
                    in  expr $ E.Let (E.VarDef σ (expr e')) ebody

                Predicate p ->
                    let p' = desugar p
                        σ  = ElimConstr (M.fromFoldable [ cTrue  × Body (go es n)
                                                        , cFalse × Body (expr $ E.Constr cNil L.Nil)])
                    in  expr $ E.MatchAs p' σ
        go L.Nil _ = error absurd
desugar (SExpr α (Var x))              = Expr α (E.Var x)
desugar (SExpr α (Op op))              = Expr α (E.Op op)
desugar (SExpr α (Str s))              = Expr α (E.Str s)
desugar (SExpr α (Constr ctr args))    = Expr α (E.Constr ctr (map desugar args))
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
bound_vars (Expr _ (E.Constr ctr args)) κ
    = case args of
        (e:es) -> let f :: (Cont -> Elim) -> Expr -> (Cont -> Elim)
                      f κ_cont e' = \(κ' :: Cont) -> (κ_cont $ Arg $ bound_vars e' κ')

                      z :: Cont -> Elim
                      z = bound_vars e

                  in  ElimConstr (M.fromFoldable [ctr × (Arg $ (foldl f z es) κ)])

        L.Nil ->  ElimConstr M.empty
bound_vars _ _ = error absurd
