module Test.Desugar where

import Prelude
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Bindings (Var)
import DataType (cCons, cNil)
import Lattice (𝔹)
import SExpr (Pattern(..), Qualifier(..), Expr, RawExpr(..), expr)
import Util ((×))

lcomp1 :: Expr 𝔹
lcomp1
 = expr $ ListComp (expr $ BinaryApp (var "x") "+" (var "y")) $
            NonEmptyList $
               (Generator (PVar "x") $ cons (int 5) (cons (int 4) (cons (int 3) nil))) :|
               ((Generator (PVar "y") $ cons (int 9) (cons (int 7) (cons (int 5) nil))) : Nil)

lcomp2 :: Expr 𝔹
lcomp2
 = expr $ ListComp (var "z") $
            NonEmptyList $
               (Generator (PVar "x") (cons (int 5) (cons (int 4) (cons (int 3) nil))) :|
               (Generator (PVar "y") (cons (int 9) (cons (int 7) (cons (int 5) nil)))) :
               (Declaration (PVar "z" × expr (BinaryApp (var "x") "+" (var "y")))) :
               (Generator (PVar "c") (cons (int 9) (cons (int 7) (cons (int 5) nil))))
               : Nil)

lcomp3 :: Expr 𝔹
lcomp3
 = expr $ ListComp (var "z") $
            NonEmptyList $
            (Generator (PVar "x") (cons (int 5) (cons (int 4) (cons (int 3) nil))) :|
            (Generator (PVar "y") (cons (int 9) (cons (int 7) (cons (int 5) nil)))):
            (Declaration (PVar "z" × expr (BinaryApp (var "x") "+" (var "y")))) :
            (Guard (expr $ BinaryApp (var "z") "<" (int 10))):
            Nil)

lcomp4 :: Expr 𝔹
lcomp4
 = expr $ ListComp (var "x") $
            NonEmptyList $
            (Generator (PConstr cCons (PVar "x" : PVar "xs":Nil)) (cons (cons (int 5) nil) (cons (cons (int 4) nil) (cons (cons (int 3) nil) (cons nil nil)))) :|
            Nil)

lcomp1_eval :: String
lcomp1_eval = "[14, 12, 10, 13, 11, 9, 12, 10, 8]"

lcomp2_eval :: String
lcomp2_eval = "[14, 14, 14, 12, 12, 12, 10, 10, 10, 13, 13, 13, 11, 11, 11, 9, 9, 9, 12, 12, 12, 10, 10, 10, 8, 8, 8]"

lcomp3_eval :: String
lcomp3_eval = "[9, 8]"

lcomp4_eval :: String
lcomp4_eval = "[5, 4, 3]"

lseq1 :: Expr 𝔹
lseq1 = expr $ ListRange (int 3) (int 7)

lseq1_eval :: String
lseq1_eval = "[3, 4, 5, 6, 7]"

var :: Var -> Expr 𝔹
var x = expr $ Var x

cons :: Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
cons se1 se2 = expr $ Constr cCons (se1:se2:Nil)

nil :: Expr 𝔹
nil = expr $ Constr cNil Nil

int :: Int -> Expr 𝔹
int n = expr $ Int n
