module Desugar where

import Prelude hiding (absurd)
import Data.List ((:), List)
import Data.List (List(..)) as L
import Data.Map (fromFoldable, singleton, member, insert, lookup) as M
import DataType (Ctr, cCons, cNil, cTrue, cFalse, cPair)
import Expr (Cont(..), Elim(..), Expr(..), RecDefs, VarDef(..), Var, expr)
import Expr (RawExpr(..)) as E
import Lattice (𝔹)
import Util ((×), absurd, error, fromJust)

lcomp1 :: SExpr
lcomp1
 = sexpr $ ListComp (sexpr $ BinaryApp (svar "x") "+" (svar "y"))
            ((Generator (PVar "x") (scons (sint 5)
            (scons (sint 4) (scons (sint 3) (snil))))):
            (Generator (PVar "y") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil))))):
            L.Nil)

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
            :L.Nil)

lcomp3 :: SExpr
lcomp3
 = sexpr $ ListComp (svar "z")
            ((Generator (PVar "x") (scons (sint 5)
            (scons (sint 4) (scons (sint 3) (snil))))):
            (Generator (PVar "y") (scons (sint 9)
            (scons (sint 7) (scons (sint 5) (snil))))):
            (Declaration (PVar "z") (sexpr $ BinaryApp (svar "x") "+" (svar "y"))):
            (Guard (sexpr $ BinaryApp (svar "z") "<" (sint 10))):
            L.Nil)


lcomp4 :: SExpr
lcomp4
 = sexpr $ ListComp (svar "x")
            ((Generator (PConstr cCons (PVar "x":PVar "xs":L.Nil)) (scons (scons (sint 5) snil)
             (scons (scons (sint 4) snil) (scons (scons (sint 3) snil) (scons snil snil))))):
            L.Nil)

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
scons se1 se2 = sexpr $ Constr cCons (se1:se2:L.Nil)

snil :: SExpr
snil = sexpr $ Constr cNil L.Nil

sint :: Int -> SExpr
sint n = sexpr $ Int n

eapp :: Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
eapp f x = expr $ E.App f x

enil :: Expr 𝔹
enil = expr $ E.Constr cNil L.Nil

evar :: Var -> Expr 𝔹
evar x = expr $ E.Var x

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
   ListSeq SExpr SExpr |
   ListComp SExpr (List Predicate) |
   Let (VarDef 𝔹) SExpr |
   LetRec (RecDefs 𝔹) SExpr

data Pattern =
   PVar Var |
   PConstr Ctr (List Pattern)

data Predicate =
   Guard SExpr |
   Generator Pattern SExpr |
   Declaration Pattern SExpr

data SExpr =
   SExpr Boolean SugaredExpr

sexpr :: SugaredExpr -> SExpr
sexpr = SExpr false

desugar :: SExpr -> Expr 𝔹
desugar (SExpr α (Int n)) = Expr α (E.Int n)
desugar (SExpr α (IfElse s1 s2 s3))
    = let e1 = desugar s1
          e2 = desugar s2
          e3 = desugar s3
          σ = ElimConstr (M.fromFoldable [ cTrue  × Body e2
                                         , cFalse × Body e3])
      in  Expr α (E.MatchAs e1 σ)
desugar (SExpr α (ListSeq s1 s2))
   = let e1 = desugar s1
         e2 = desugar s2
     in  eapp (eapp (evar "enumFromTo") e1) e2
desugar (SExpr α (ListComp s_body (Guard (SExpr _ (Constr cTrue L.Nil)) : L.Nil )))
   = expr $ E.Constr cCons (desugar s_body : enil : L.Nil)
desugar (SExpr α (ListComp s_body (q:L.Nil)))
   = desugar (sexpr $ ListComp s_body (q : Guard (sexpr $ Constr cTrue L.Nil) : L.Nil))
desugar (SExpr α (ListComp s_body (Guard s : qs)))
   =  let e = desugar s
          σ  = ElimConstr (M.fromFoldable [ cTrue  × Body (desugar (SExpr α (ListComp s_body qs)))
                                          , cFalse × Body enil])
      in  expr $ E.MatchAs e σ
desugar (SExpr α (ListComp s_body (Generator p slist : qs)))
   =  let elist = desugar slist
          erest = desugar (sexpr $ ListComp s_body qs)
          λ     = expr $ E.Lambda (totalize (patternToElim p (Body erest)) enil)
      in  eapp (evar "concat") (eapp (eapp (evar "map") λ) elist)
desugar (SExpr α (ListComp s_body (Declaration p s : qs)))
   =  let e     = desugar s
          σ     = patternToElim p None
          erest = desugar (SExpr α (ListComp s_body qs))
      in  expr $ E.Let (VarDef σ e) erest
desugar (SExpr α (ListComp s_body _))
   =  error absurd
desugar (SExpr α (Var x))              = Expr α (E.Var x)
desugar (SExpr α (Op op))              = Expr α (E.Op op)
desugar (SExpr α (Str s))              = Expr α (E.Str s)
desugar (SExpr α (Constr ctr args))    = Expr α (E.Constr ctr (map desugar args))
desugar (SExpr α (Lambda σ))           = Expr α (E.Lambda σ)
desugar (SExpr α (App s1 s2))          = Expr α (E.App (desugar s1) (desugar s2))
desugar (SExpr α (BinaryApp s1 op s2)) = Expr α (E.BinaryApp (desugar s1) op (desugar s2))
desugar (SExpr α (MatchAs s σ))        = Expr α (E.MatchAs (desugar s) σ)
desugar (SExpr α (Let def s))          = Expr α (E.Let def (desugar s))
desugar (SExpr α (LetRec δ s))         = Expr α (E.LetRec δ (desugar s))

patternToElim :: Pattern -> Cont 𝔹 -> Elim 𝔹
patternToElim (PVar x) κ
   = ElimVar x κ
patternToElim (PConstr ctr ps) κ
   = let go (p':p'':ps') = Arg (patternToElim p' (go (p'':ps')))
         go (p':L.Nil)   = Arg (patternToElim p' κ)
         go L.Nil        = κ
     in  ElimConstr (M.singleton ctr (go ps))

totalize :: Elim 𝔹 -> Expr 𝔹 -> Elim 𝔹
totalize (ElimConstr m) e
   | M.member cTrue m && not (M.member cFalse) m
      = ElimConstr (M.insert cFalse (Body e) m)
   | M.member cFalse m && not (M.member cTrue) m
      = ElimConstr (M.insert cTrue (Body e) m)
   | M.member cNil m && not (M.member cCons) m
      = ElimConstr (M.insert cCons (Body e) m)
   | M.member cCons m && not (M.member cNil) m
      = let cons_κ = case fromJust "" (M.lookup cCons m) of
                        Arg σ   -> Arg (totalize σ e)
                        Body e' -> Body e'
                        None    -> Body e
            nil_κ  = Body e
        in  ElimConstr (M.fromFoldable ((cCons × cons_κ):(cNil × nil_κ):L.Nil))
   | M.member cPair m
      = let pair_κ = case fromJust "" (M.lookup cPair m) of
                        Arg σ   -> Arg (totalize σ e)
                        Body e' -> Body e'
                        None    -> Body e
        in  ElimConstr (M.singleton cPair pair_κ)
   | otherwise = ElimConstr m
totalize (ElimVar e k) e'
   = case k of Arg σ  -> ElimVar e (Arg (totalize σ e'))
               Body _ -> ElimVar e k
               None   -> ElimVar e (Body e')