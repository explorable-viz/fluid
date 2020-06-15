module Desugar where

import Prelude hiding (absurd)
import Data.List (List)
import Data.Map (fromFoldable)
import Expr (RawExpr(..)) as E
import Expr (Cont(..), Elim(..), Expr(..), RawExpr, Def, RecDefs)
import Bindings (Var)
import DataType (Ctr(..))
import Util ((×), absurd, error)

data SugaredExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List Expr) |
   True | False |
   Pair Expr Expr |
   Nil | Cons SugaredExpr SugaredExpr |
   Lambda Elim |
   App Expr Expr |
   BinaryApp Expr Var Expr |
   MatchAs Expr Elim |
   IfElse Expr Expr Expr |
   ListSeq Int Int |
   ListComp Expr (List ListCompExpr) |
   Let Def Expr |
   LetRec RecDefs Expr

data ListCompExpr = Predicate SugaredExpr | InputList SugaredExpr SugaredExpr

desugar :: SugaredExpr -> RawExpr
desugar (IfElse e1 e2 e3)
    = let σ = ElimConstr (fromFoldable [ (Ctr "True")  × CExpr e2
                                       , (Ctr "False") × CExpr e3])
      in E.MatchAs e1 σ
desugar (ListSeq a z)
    | a <= z    = go z E.Nil
    where go n acc = let acc' = E.Cons (Expr false (E.Int n)) (Expr false acc)
                     in  if n == a then acc' else go (n - 1) acc'
    | otherwise = error absurd


desugar _ = error absurd