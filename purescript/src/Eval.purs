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
    _, ElimVar { x, tx, e }
        ->  Just $ T3 (Empty :+: Bind x val) e (MatchVar x)
    -- true
    ValTrue, ElimBool { btrue: e1, bfalse: _ }
        ->  Just $ T3 Empty e1 MatchTrue
    -- false
    ValFalse, ElimBool { btrue: _, bfalse: e2 }
        ->  Just $ T3 Empty e2 MatchFalse
    -- pair
    ValPair v v', ElimPair { x, y, e }
        ->  let ρ' = Empty :+: Bind x v :+: Bind y v'
            in  Just $ T3 ρ' e (MatchPair x y)
    -- nil
    ValNil, ElimList { bnil: e, bcons: _ }
        ->  Just $ T3 Empty e MatchNil
    -- cons
    ValCons v v', ElimList { bnil: _, bcons: { x, y, e } }
        ->  let ρ' = Empty :+: Bind x v :+: Bind y v'
            in  Just $ T3 ρ' e (MatchCons x y)
    -- failure
    _, _ ->  Nothing


data ExplVal = ExplVal { t :: Expl, v :: Val }


eval :: Partial => Env -> Expr -> ExplVal
-- bot
eval _ ExprBottom = ExplVal { t: ExplBottom, v: ValBottom }
-- var
eval ρ (ExprVar x)
 = case find x ρ of
    Just val -> ExplVal { t: ExplVar x,  v: val }
    _        -> ExplVal { t: ExplBottom, v: ValFailure ("variable " <> x <> " not found") }
-- true
eval ρ ExprTrue               = ExplVal { t: ExplTrue, v: ValTrue }
-- false
eval ρ ExprFalse              = ExplVal { t: ExplFalse, v: ValFalse }
-- int
eval ρ (ExprInt n)            = ExplVal { t: ExplInt n, v: ValInt n }
-- pair
eval ρ (ExprPair e1 e2)
 = let ExplVal { t: t1, v: v1 } = eval ρ e1
       ExplVal { t: t2, v: v2 } = eval ρ e2
   in  ExplVal { t: ExplPair t1 t2, v: ValPair v1 v2 }
-- nil
eval ρ ExprNil               = ExplVal { t: ExplNil, v: ValNil }
-- cons
eval ρ (ExprCons e e')
 = let ExplVal { t: t1, v: v1 } = eval ρ e
       ExplVal { t: t2, v: v2 } = eval ρ e'
   in  ExplVal { t: ExplCons t1 t2, v: ValCons v1 v2 }
-- letrec (fun)
eval ρ (ExprLetrec f σ e)     = let ExplVal {t, v} = eval (ρ :+: Bind f (ValClosure ρ f σ)) e
                                    t'     = ExplLetrec f (ExplFun ρ σ) t
                                 in ExplVal {t: t', v}
-- apply
eval ρ (ExprApp e e')
 = case eval ρ e  of
     ExplVal { t, v: ValClosure ρ' fun σ }
        -> let ExplVal { t: t',  v } = eval ρ e'
           in case match v σ of
                Just (T3 ρ'' e'' m) -> let ExplVal { t: u, v: v' } = eval (conc ρ' ρ'' :+: Bind fun (ValClosure ρ' fun σ)) e''
                                       in  ExplVal { t: ExplApp t t' m u, v: v' }
                Nothing           -> ExplVal { t: ExplBottom, v: ValFailure "Match not found" }
     _  -> ExplVal { t: ExplBottom, v: ValFailure "Expression does not evaluate to closure" }
-- add
eval ρ (ExprAdd e1 e2)
 = let ExplVal { t: t1, v: v1 } = eval ρ e1
       ExplVal { t: t2, v: v2 } = eval ρ e2
   in  case v1, v2 of
          ValInt n1, ValInt n2 -> ExplVal { t: ExplAdd t1 t2, v: ValInt (n1 + n2) }
          _,          _        -> ExplVal { t: ExplBottom, v: ValFailure "Arithmetic type error: e1 or/and e2 do not evaluate to ints" }
-- let
eval ρ (ExprLet x e1 e2)
 = let ExplVal { t: t1, v: v1 } = eval ρ e1
       ρ'  = (ρ :+: Bind x v1)
       ExplVal { t: t2, v: v2 }  = eval ρ' e2
   in  ExplVal {t: ExplLet x t1 t2, v: v2 }
-- match (no rule)
eval ρ (ExprMatch e σ)
 = let ExplVal { t: t1, v: v1 } = eval ρ e
   in case match v1 σ of
        Nothing            -> ExplVal { t: ExplBottom, v: ValFailure "Match not found" }
        Just (T3 ρ' e' m)  -> let ExplVal { t: t2, v: v2 } = eval (conc ρ ρ') e'
                              in  ExplVal { t: ExplMatch t1 m t2, v: v2 }
