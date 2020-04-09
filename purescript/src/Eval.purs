module Eval where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Expl (Match(..), Expl(..))
import Expr 


match :: Val -> Elim -> Maybe (T3 Env Expr Match)
match val σ
 = case  val, σ of
    -- var
    _, ElimVar x t e
        ->  Just $ T3 (Empty :+: Bind x val) e (MatchVar x)
    -- true
    ValTrue, ElimBool (BranchTrue e1) (BranchFalse _)
        ->  Just $ T3 Empty e1 MatchTrue
    -- false
    ValFalse, ElimBool (BranchTrue _) (BranchFalse e2)
        ->  Just $ T3 Empty e2 MatchFalse
    -- pair
    ValPair v v', ElimPair x _ y _ e
        ->  let ρ' = Empty :+: Bind x v :+: Bind y v' 
            in  Just $ T3 ρ' e (MatchPair x y)
    -- nil
    ValNil, ElimList (BranchNil e2) (BranchCons _ _ _ _)
        ->  Just $ T3 Empty e2 MatchNil
    -- cons
    ValCons v v', ElimList (BranchNil _) (BranchCons x y _ e1)
        ->  let ρ' = (Empty :+: Bind x v :+: Bind y v' )
            in  Just $ T3 ρ' e1 (MatchCons x y)
    -- failure
    _, _ ->  Nothing


type ExplVal = { t :: Expl, v :: Val }


eval :: Partial => Expr -> Env -> ExplVal
-- bot
eval ExprBottom ρ = { t: ExplBottom, v: ValBottom }
-- var
eval (ExprVar x) ρ
 = case find x ρ of
    Just val -> { t: ExplVar x,  v: val }
    _        -> { t: ExplBottom, v: ValFailure ("variable " <> x <> " not found") }
-- true
eval ExprTrue  ρ             = { t: ExplTrue, v: ValTrue }
-- false
eval ExprFalse  ρ            = { t: ExplFalse, v: ValFalse }
-- int
eval (ExprInt n)  ρ          = { t: ExplInt n, v: ValInt n }
-- pair
eval (ExprPair e1 e2)  ρ
 = let { t: t1, v: v1 } = eval e1 ρ
       { t: t2, v: v2 } = eval e2 ρ
   in  { t: ExplPair t1 t2, v: ValPair v1 v2 }
-- nil
eval ExprNil ρ               = { t: ExplNil, v: ValNil }
-- cons
eval (ExprCons e es)  ρ
 = let { t: t1, v: v1 } = eval e ρ
       { t: t2, v: v2 } = eval es ρ
   in  { t: ExplCons t1 t2, v: ValCons v1 v2 }
-- letrec (fun)
eval (ExprLetrec fun σ e)  ρ   = let {t, v} = eval e (ρ :+: Bind fun (ValClosure ρ fun σ))
                                     t'     = ExplLetrec fun (ExplFun ρ σ) t
                                 in {t: t', v}
-- apply
eval (ExprApp e e')  ρ
 = case eval e ρ  of
     { t, v: ValClosure ρ' fun σ }
        -> let { t: t',  v } = eval e' ρ
           in case match v σ of
                Just (T3 ρ'' e'' m) -> let { t: u, v: v' } = eval e'' (conc ρ' ρ'' :+: Bind fun (ValClosure ρ' fun σ))
                                       in  { t: ExplApp t t' m u, v: v' }
                Nothing           -> { t: ExplBottom, v: ValFailure "Match not found" }
     _  -> { t: ExplBottom, v: ValFailure "Applied expression e in e e' does not evaluate to closure" }
-- add
eval (ExprAdd e1 e2) ρ
 = let { t: t1, v: v1 } = eval e1 ρ
       { t: t2, v: v2 } = eval e2 ρ
   in  case v1, v2 of
          ValInt n1, ValInt n2 -> { t: ExplAdd t1 t2, v: ValInt (n1 + n2) }
          _,          _        -> { t: ExplBottom, v: ValFailure "Arithmetic type error: e1 or/and e2 do not evaluate to ints" }
-- let
eval (ExprLet x e1 e2) ρ
 = let { t: t1, v: v1 } = eval e1 ρ
       ρ'  = (ρ :+: Bind x v1)
       { t: t2, v: v2 }  = eval e2 ρ'
   in  {t: ExplLet x t1 t2, v: v2 }
-- match (no rule)
eval (ExprMatch e σ) ρ
 = let { t: t1, v: v1 } = eval e ρ
   in case match v1 σ of
        Nothing            -> { t: ExplBottom, v: ValFailure "Match not found" }
        Just (T3 ρ' e' m)  -> let { t: t2, v: v2 } = eval e' (conc ρ ρ')
                              in  { t: ExplMatch t1 m t2, v: v2 }
