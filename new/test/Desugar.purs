module Test.Desugar where

import Prelude
import Data.List (List(..), (:))
import Bindings (Var)
import DataType (cCons, cNil)
import SExpr (Expr, RawExpr(..), expr)
import Lattice (𝔹)

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
