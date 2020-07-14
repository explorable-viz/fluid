module Desugar where

import Prelude hiding (absurd)
import Data.Foldable (foldl)
import Data.List ((:), List)
import Data.List (List(..)) as L
import Data.Map (fromFoldable, empty, singleton) as M
import DataType (Ctr, cCons, cNil, cTrue, cPair, cFalse)
import Expr (Cont(..), Elim(..), Expr(..), RecDefs, VarDef(..), Var, expr)
import Expr (RawExpr(..)) as E
import Lattice (𝔹)
import Util ((×), absurd, error)

lcomp2 :: SExpr
lcomp2
 = sexpr $ ListComp (sexpr $ BinaryApp (svar "x") "+" (svar "y"))
            ((Generator (PVar "x") (scons (sint 5)
            (scons (sint 4) (scons (sint 3) (snil))))):
            (Generator (PVar "y") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil))))):
            L.Nil)

lcomp3 :: SExpr
lcomp3
 = sexpr $ ListComp (svar "z")
            ((Generator (PVar "x") (scons (sint 5)
            (scons (sint 4) (scons (sint 3) (snil))))):
            (Generator (PVar "y") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil))))):
            (Declaration (PVar "z") (sexpr $ BinaryApp (svar "x") "+" (svar "y")))
            :L.Nil)

lcomp3_eval :: String
lcomp3_eval = "[14, 12, 10, 13, 11, 9, 12, 10, 8]"

svar :: Var -> SExpr
svar x = sexpr $ Var x

scons :: SExpr -> SExpr -> SExpr
scons se1 se2 = sexpr $ Constr cCons (se1:se2:L.Nil)

snil :: SExpr
snil = sexpr $ Constr cNil L.Nil

sint :: Int -> SExpr
sint n = sexpr $ Int n

data SugaredExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List SExpr) |
   Lambda (Elim 𝔹) |
   App SExpr SExpr |
   BinaryApp SExpr Var SExpr |
   MatchAs SExpr (Elim 𝔹) |
   IfElse SExpr SExpr SExpr |
   ListSeq Int Int |
   ListComp SExpr (List Predicate) |
   Let (VarDef 𝔹) SExpr |
   LetRec (RecDefs 𝔹) SExpr

data Pattern = PVar Var | PPair Pattern Pattern | PNil | PCons Pattern Pattern

data Predicate = Guard SExpr | Generator Pattern SExpr | Declaration Pattern SExpr

data SExpr = SExpr Boolean SugaredExpr

sexpr :: SugaredExpr -> SExpr
sexpr = SExpr false

desugar :: SExpr -> Expr 𝔹
desugar (SExpr α (Int n)) = Expr α (E.Int n)
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
        go :: List Predicate -> Expr 𝔹
        go (s:L.Nil)
            = case s of
                Generator pattern input_list ->
                    let list_expr   = desugar input_list
                        λ           = expr $ E.Lambda (patternToElim pattern (Body $ desugar s_lhs))
                    in  expr $ E.App (expr $ E.App (expr $ E.Var "map") λ) list_expr

                Guard g ->
                    let g' = desugar g
                        σ  = ElimConstr (M.fromFoldable [ cTrue  × Body (desugar s_lhs)
                                                        , cFalse × Body (expr $ E.Constr cNil L.Nil)])
                    in  expr $ E.MatchAs g' σ

                Declaration pattern se ->
                    let e  = desugar se
                        σ  = patternToElim pattern None
                    in  expr $ E.Let (VarDef σ e) (desugar s_lhs)
        go (s:s':ss)
            = case s of
                Generator pattern input_list ->
                    let list_expr   = desugar input_list
                        λ           = expr $ E.Lambda (patternToElim pattern (Body $ go (s':ss)))
                        maybeConcat = case s' of Generator _ _ -> \x -> expr $ E.App (expr $ E.Var "concat") x
                                                 _ -> \x -> x
                    in  maybeConcat (expr $ E.App (expr $ E.App (expr $ E.Var "map") λ) list_expr)

                Guard g ->
                    let g' = desugar g
                        σ  = ElimConstr (M.fromFoldable [ cTrue  × Body (go (s':ss))
                                                        , cFalse × Body (expr $ E.Constr cNil L.Nil)])
                    in  expr $ E.MatchAs g' σ

                Declaration pattern se ->
                    let e  = desugar se
                        σ  = patternToElim pattern None
                    in  expr $ E.Let (VarDef σ e) (go (s':ss))
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

patternToElim :: Pattern -> Cont 𝔹 -> Elim 𝔹
patternToElim (PVar x) κ
    = ElimVar x κ
patternToElim (PPair p1 p2) κ
    = let σ  = patternToElim p2 κ
          σ' = patternToElim p1 (Arg σ)
      in  ElimConstr (M.singleton cPair (Arg σ'))
patternToElim (PNil) κ
    = ElimConstr (M.singleton cNil κ)
patternToElim (PCons p1 p2) κ
    = let  σ = patternToElim p2 κ
           σ' = patternToElim p1 (Arg σ)
      in   ElimConstr (M.singleton cCons (Arg σ'))
-- patternToElim _ _ = error absurd
-- patternToElim (Expr _ (E.Constr ctr args)) κ
--     = case args of
--         (e:es) -> let f :: (Cont 𝔹 -> Elim 𝔹) -> Expr 𝔹 -> (Cont 𝔹 -> Elim 𝔹)
--                       f κ_cont e' = \(κ' :: Cont 𝔹) -> (κ_cont $ Arg $ bindingToElim e' κ')

--                       z :: Cont 𝔹 -> Elim 𝔹
--                       z = bindingToElim e

--                   in  ElimConstr (M.fromFoldable [ctr × (Arg $ (foldl f z es) κ)])

--         L.Nil ->  ElimConstr M.empty

