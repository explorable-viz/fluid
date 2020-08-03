module Test.Desugar where

import Prelude
import Data.List (List(..), (:))
import Bindings (Var)
import DataType (cCons, cNil)
import Desugar (Pattern(..), Predicate(..), SExpr, RawSExpr(..), sexpr)

lcomp1 :: SExpr
lcomp1
 = sexpr $ ListComp (sexpr $ BinaryApp (svar "x") "+" (svar "y"))
            ((Generator (PVar "x") (scons (sint 5)
            (scons (sint 4) (scons (sint 3) (snil))))):
            (Generator (PVar "y") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil))))):
            Nil)

lcomp2 :: SExpr
lcomp2
 = sexpr $ ListComp (svar "z")
            ((Generator (PVar "x") (scons (sint 5)
            (scons (sint 4) (scons (sint 3) (snil))))):
            (Generator (PVar "y") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil))))):
            (Declaration (PVar "z") (sexpr $ BinaryApp (svar "x") "+" (svar "y"))):
            (Generator (PVar "c") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil)))))
            :Nil)

lcomp3 :: SExpr
lcomp3
 = sexpr $ ListComp (svar "z")
            ((Generator (PVar "x") (scons (sint 5)
            (scons (sint 4) (scons (sint 3) (snil))))):
            (Generator (PVar "y") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil))))):
            (Declaration (PVar "z") (sexpr $ BinaryApp (svar "x") "+" (svar "y"))):
            (Guard (sexpr $ BinaryApp (svar "z") "<" (sint 10))):
            Nil)


lcomp4 :: SExpr
lcomp4
 = sexpr $ ListComp (svar "x")
            ((Generator (PConstr cCons (PVar "x":PVar "xs":Nil)) (scons (scons (sint 5) snil)
             (scons (scons (sint 4) snil) (scons (scons (sint 3) snil) (scons snil snil))))):
            Nil)

lcomp1_eval :: String
lcomp1_eval = "[14, 12, 10, 13, 11, 9, 12, 10, 8]"

lcomp2_eval :: String
lcomp2_eval = "[14, 14, 14, 12, 12, 12, 10, 10, 10, 13, 13, 13, 11, 11, 11, 9, 9, 9, 12, 12, 12, 10, 10, 10, 8, 8, 8]"

lcomp3_eval :: String
lcomp3_eval = "[9, 8]"

lcomp4_eval :: String
lcomp4_eval = "[5, 4, 3]"

lseq1 :: SExpr
lseq1 = sexpr $ ListSeq (sint 3) (sint 7)

lseq1_eval :: String
lseq1_eval = "[3, 4, 5, 6, 7]"

svar :: Var -> SExpr
svar x = sexpr $ Var x

scons :: SExpr -> SExpr -> SExpr
scons se1 se2 = sexpr $ Constr cCons (se1:se2:Nil)

snil :: SExpr
snil = sexpr $ Constr cNil Nil

sint :: Int -> SExpr
sint n = sexpr $ Int n
