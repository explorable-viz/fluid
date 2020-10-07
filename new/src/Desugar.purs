module Desugar where

import Prelude hiding (absurd)
import Data.List (List(..), (:), (\\), head)
import Data.Map (fromFoldable, singleton, toUnfoldable) as M
import Data.Tuple (fst)
import DataType (Ctr, DataType'(..), ctrToDataType, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), Expr(..), RecDefs, VarDef(..), Var, expr)
import Expr (RawExpr(..)) as E
import Lattice (𝔹)
import Util (type (×), (×), absurd, error, fromJust, mustLookup)

data RawSExpr =
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

type Branch = Pattern × Expr 𝔹

data Predicate =
   Guard SExpr |
   Generator Pattern SExpr |
   Declaration Pattern SExpr

data SExpr =
   SExpr 𝔹 RawSExpr

sexpr :: RawSExpr -> SExpr
sexpr = SExpr false

eapp :: Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
eapp f = expr <<< E.App f

enil :: Expr 𝔹
enil = expr $ E.Constr cNil Nil

evar :: Var -> Expr 𝔹
evar = expr <<< E.Var

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
     in  eapp (eapp (evar "range") e1) e2
desugar (SExpr α (ListComp s_body (Guard (SExpr _ (Constr cTrue Nil)) : Nil )))
   = expr $ E.Constr cCons (desugar s_body : enil : Nil)
desugar (SExpr α (ListComp s_body (q:Nil)))
   = desugar (sexpr $ ListComp s_body (q : Guard (sexpr $ Constr cTrue Nil) : Nil))
desugar (SExpr α (ListComp s_body (Guard s : qs)))
   =  let e = desugar s
          σ  = ElimConstr (M.fromFoldable [ cTrue  × Body (desugar (SExpr α (ListComp s_body qs)))
                                          , cFalse × Body enil])
      in  expr $ E.MatchAs e σ
desugar (SExpr α (ListComp s_body (Generator p slist : qs)))
   =  let elist = desugar slist
          erest = desugar (sexpr $ ListComp s_body qs)
          λ     = expr $ E.Lambda (totalise (patternToElim p (Body erest)) enil)
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
   = let go (p':p'':ps')   = Arg (patternToElim p' (go (p'':ps')))
         go (p':Nil)       = Arg (patternToElim p' κ)
         go Nil            = κ
     in  ElimConstr (M.singleton ctr (go ps))

totalise :: Elim 𝔹 -> Expr 𝔹 -> Elim 𝔹
totalise (ElimConstr m) e
   = let ctr × κ              = fromJust "" (head $ M.toUnfoldable m)
         branches             = M.toUnfoldable m
         DataType _ sigs      = mustLookup ctr ctrToDataType
         all_ctrs             = fst <$> M.toUnfoldable sigs
         new_branches         = (_ × Body e) <$> (all_ctrs \\ (fst <$> branches))
         totalised_branches   = branches <#>
                                 \(c × κ) -> case mustLookup c m of
                                                Arg σ   -> c × Arg (totalise σ e)
                                                Body e' -> c × Body e'
                                                None    -> c × Body e
     in   ElimConstr (M.fromFoldable $ totalised_branches <> new_branches)
totalise (ElimVar e k) e'
   = case k of Arg σ  -> ElimVar e $ Arg (totalise σ e')
               Body _ -> ElimVar e k
               None   -> ElimVar e $ Body e'
