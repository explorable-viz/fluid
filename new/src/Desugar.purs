module Desugar where

import Prelude hiding (absurd)
import Data.Foldable (foldl)
import Data.List ((:), List)
import Data.List (List(..)) as L
import Data.Map (fromFoldable, empty) as M
import Debug.Trace (trace) as T
import DataType (Ctr, cCons, cNil, cPair, cTrue, cFalse)
import Expr (Cont(..), Elim(..), Expr(..), RecDefs, VarDef, Var, expr)
import Expr (RawExpr(..)) as E
import Lattice (𝔹)
import Pretty (pretty)
import Util ((×), absurd, error)

trace s a = T.trace (pretty s) $ \_-> a
trace' s a = T.trace  s $ \_-> a

data SugaredExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List SExpr) |
   True | False |
   Pair SExpr SExpr |
   Nil | Cons SExpr SExpr |
   Lambda (Elim 𝔹) |
   App SExpr SExpr |
   BinaryApp SExpr Var SExpr |
   MatchAs SExpr (Elim 𝔹) |
   IfElse SExpr SExpr SExpr |
   ListSeq Int Int |
   ListComp SExpr (List ListCompExpr) |
   Let (VarDef 𝔹) SExpr |
   LetRec (RecDefs 𝔹) SExpr

data ListCompExpr = Predicate SExpr | InputList SExpr SExpr

data SExpr = SExpr Boolean SugaredExpr

sexpr :: SugaredExpr -> SExpr
sexpr = SExpr false

lcomp1 :: SExpr
lcomp1 = sexpr $ ListComp (sexpr $ Var "x")
                 ((InputList (sexpr $ Var "x") (sexpr $ Cons (sexpr $ Int 5)
                    (sexpr $ Cons (sexpr $ Int 4) (sexpr $ Cons (sexpr $ Int 3) (sexpr $ Nil))))):L.Nil)
lcomp2 :: SExpr
lcomp2 = sexpr $ ListComp (sexpr $ Var "x")
                 ((InputList (sexpr $ Var "x") (sexpr $ Cons (sexpr $ Int 5)
                    (sexpr $ Cons (sexpr $ Int 4) (sexpr $ Cons (sexpr $ Int 3) (sexpr $ Nil))))):
                        (Predicate (sexpr $ BinaryApp (sexpr $ Var "x") "<" (sexpr $ Int 5))):L.Nil)

lcomp3 :: SExpr
lcomp3 = sexpr $ ListComp (sexpr $ BinaryApp (sexpr $ Var "x") "+" (sexpr $ Var "y"))
                 ((InputList (sexpr $ Var "x") (sexpr $ Cons (sexpr $ Int 5)
                    (sexpr $ Cons (sexpr $ Int 4) (sexpr $ Cons (sexpr $ Int 3) (sexpr $ Nil))))):
                    (InputList (sexpr $ Var "y") (sexpr $ Cons (sexpr $ Int 9)
                    (sexpr $ Cons (sexpr $ Int 7) (sexpr $ Cons (sexpr $ Int 5) (sexpr $ Nil))))):L.Nil)

lcomp3_pretty :: String
lcomp3_pretty = "[14, 12, 10, 13, 11, 9, 12, 10, 8]"

lcomp4 :: SExpr
lcomp4 = sexpr $ ListComp (sexpr $ BinaryApp (sexpr $ Var "x") "+" (sexpr $ Var "y"))
                 ((InputList (sexpr $ Var "x") (sexpr $ Cons (sexpr $ Int 5)
                  (sexpr $ Cons (sexpr $ Int 4) (sexpr $ Cons (sexpr $ Int 3) (sexpr $ Nil))))):
                 (InputList (sexpr $ Var "y") (sexpr $ Cons (sexpr $ Int 9)
                  (sexpr $ Cons (sexpr $ Int 7) (sexpr $ Cons (sexpr $ Int 5) (sexpr $ Nil))))):
                 (InputList (sexpr $ Var "z") (sexpr $ Cons (sexpr $ Int 12)
                  (sexpr $ Cons (sexpr $ Int 2) (sexpr $ Cons (sexpr $ Int 13) (sexpr $ Nil))))):
                    L.Nil)

lcomp5 :: SExpr
lcomp5 = sexpr $ ListComp (sexpr $ BinaryApp (sexpr $ BinaryApp (sexpr $ Var "x") "+" (sexpr $ Var "y")) "+" (sexpr $ Var "a"))
                 ((InputList (sexpr $ Var "x") (sexpr $ Cons (sexpr $ Int 5)
                  (sexpr $ Cons (sexpr $ Int 4) (sexpr $ Cons (sexpr $ Int 3) (sexpr $ Nil))))):
                 (InputList (sexpr $ Var "y") (sexpr $ Cons (sexpr $ Int 9)
                  (sexpr $ Cons (sexpr $ Int 7) (sexpr $ Cons (sexpr $ Int 5) (sexpr $ Nil))))):
                 (InputList (sexpr $ Pair (sexpr $ Var "a") (sexpr $ Var "b")) (sexpr $ Cons (sexpr $ Pair (sexpr $ Int 3) (sexpr $ Int 12))
                  (sexpr $ Cons (sexpr $ Pair (sexpr $ Int 2) (sexpr $ Int 0)) (sexpr $ Cons (sexpr $ Pair (sexpr $ Int 3) (sexpr $ Int 7)) (sexpr $ Nil))))):
                    L.Nil)

lcomp5_pretty :: String
lcomp5_pretty = "[17, 16, 17, 15, 14, 15, 13, 12, 13, 16, 15, 16, 14, 13, 14, 12, 11, 12, 15, 14, 15, 13, 12, 13, 11, 10, 11]"

lcomp6 :: SExpr
lcomp6 = sexpr $ ListComp (sexpr $ BinaryApp (sexpr $ Var "x") "+" (sexpr $ Var "y"))
                 ((InputList (sexpr $ Var "x") (sexpr $ Cons (sexpr $ Int 5)
                  (sexpr $ Cons (sexpr $ Int 4) (sexpr $ Cons (sexpr $ Int 3) (sexpr $ Nil))))):
                 (InputList (sexpr $ Var "y") (sexpr $ Cons (sexpr $ Int 9)
                  (sexpr $ Cons (sexpr $ Int 7) (sexpr $ Cons (sexpr $ Int 5) (sexpr $ Nil))))):
                  (Predicate (sexpr $ BinaryApp (sexpr $ Var "y") "<" (sexpr $ Int 8))):
                 (InputList (sexpr $ Var "z") (sexpr $ Cons (sexpr $ Int 12)
                  (sexpr $ Cons (sexpr $ Int 2) (sexpr $ Cons (sexpr $ Int 13) (sexpr $ Nil))))):
                    L.Nil)

lcomp6_pretty :: String
lcomp6_pretty = "[12, 12, 12, 10, 10, 10, 11, 11, 11, 9, 9, 9, 10, 10, 10, 8, 8, 8]"

desugar :: SExpr -> Expr 𝔹
desugar (SExpr α (Int n)) = Expr α (E.Int n)
desugar (SExpr α True) = Expr α (E.Constr cTrue L.Nil)
desugar (SExpr α False) = Expr α (E.Constr cFalse L.Nil)
desugar (SExpr α (Pair s1 s2)) = Expr α (E.Constr cPair ((desugar s1):(desugar s2):L.Nil))
desugar (SExpr α Nil) = Expr α (E.Constr cNil L.Nil)
desugar (SExpr α (Cons s1 s2)) = Expr α (E.Constr cCons ((desugar s1):(desugar s2):L.Nil))
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
desugar (SExpr α (ListComp s_lhs s_rhs))
    = go s_rhs
    where
        go :: List ListCompExpr -> Expr 𝔹
        go (s:L.Nil)
            = case s of
                InputList bound_var input_list ->
                    let bound_expr  = desugar bound_var
                        list_expr   = desugar input_list
                        λ           = expr $ E.Lambda (bindingToElim (bound_expr) (Body $ desugar s_lhs))
                    in  expr $ E.App (expr $ E.App (expr $ E.Var "map") λ) list_expr

                Predicate p ->
                    let p' = desugar p
                        σ  = ElimConstr (M.fromFoldable [ cTrue  × Body (desugar s_lhs)
                                                        , cFalse × Body (expr $ E.Constr cNil L.Nil)])
                    in  expr $ E.MatchAs p' σ
        go (s:ss)
            = case s of
                InputList bound_var input_list ->
                    let bound_expr  = desugar bound_var
                        list_expr   = desugar input_list
                        λ           = expr $ E.Lambda (bindingToElim bound_expr (Body $ go ss))
                    in  expr $ E.App (expr $ E.Var "concat") (expr $ E.App (expr $ E.App (expr $ E.Var "map") λ) list_expr)

                Predicate p ->
                    let p' = desugar p
                        σ  = ElimConstr (M.fromFoldable [ cTrue  × Body (go ss)
                                                        , cFalse × Body (expr $ E.Constr cNil L.Nil)])
                    in  expr $ E.MatchAs p' σ
        go L.Nil  = error absurd
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

bindingToElim :: Expr 𝔹 -> Cont 𝔹 -> Elim 𝔹
bindingToElim (Expr _ (E.Var x)) κ
    = ElimVar x κ
bindingToElim (Expr _ (E.Constr ctr args)) κ
    = case args of
        (e:es) -> let f :: (Cont 𝔹 -> Elim 𝔹) -> Expr 𝔹 -> (Cont 𝔹 -> Elim 𝔹)
                      f κ_cont e' = \(κ' :: Cont 𝔹) -> (κ_cont $ Arg $ bindingToElim e' κ')

                      z :: Cont 𝔹 -> Elim 𝔹
                      z = bindingToElim e

                  in  ElimConstr (M.fromFoldable [ctr × (Arg $ (foldl f z es) κ)])

        L.Nil ->  ElimConstr M.empty
bindingToElim _ _ = error absurd
