module Eval where

import Prelude ((<>), ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Bindings (Bind(..), Bindings(..), (:+:), conc, find)
import Expl (Match(..), Expl(..))
import Expr
import Val (Env, Val)
import Val (Val(..)) as V

match :: Val -> Elim -> Maybe (T3 Env Expr Match)
match val σ
 = case  val, σ of
    -- var
    _, ElimVar { x, tx, e }
        ->  Just $ T3 (Empty :+: Bind x val) e (MatchVar x)
    -- true
    V.True, ElimBool { btrue: e1, bfalse: _ }
        ->  Just $ T3 Empty e1 MatchTrue
    -- false
    V.False, ElimBool { btrue: _, bfalse: e2 }
        ->  Just $ T3 Empty e2 MatchFalse
    -- pair
    V.Pair v v', ElimPair { x, y, e }
        ->  let ρ' = Empty :+: Bind x v :+: Bind y v'
            in  Just $ T3 ρ' e (MatchPair x y)
    -- nil
    V.Nil, ElimList { bnil: e, bcons: _ }
        ->  Just $ T3 Empty e MatchNil
    -- cons
    V.Cons v v', ElimList { bnil: _, bcons: { x, y, e } }
        ->  let ρ' = Empty :+: Bind x v :+: Bind y v'
            in  Just $ T3 ρ' e (MatchCons x y)
    -- failure
    _, _ ->  Nothing


data ExplVal = ExplVal { t :: Expl, v :: Val }


eval :: Partial => Env -> Expr -> ExplVal
-- var
eval ρ (Var x)
 = case find x ρ of
    Just val -> ExplVal { t: ExplVar x,  v: val }
    _        -> ExplVal { t: ExplBottom, v: V.Failure ("variable " <> x <> " not found") }
-- true
eval ρ True               = ExplVal { t: ExplTrue, v: V.True }
-- false
eval ρ False              = ExplVal { t: ExplFalse, v: V.False }
-- int
eval ρ (Int n)            = ExplVal { t: ExplInt n, v: V.Int n }
-- pair
eval ρ (Pair e1 e2)
 = let ExplVal { t: t1, v: v1 } = eval ρ e1
       ExplVal { t: t2, v: v2 } = eval ρ e2
   in  ExplVal { t: ExplPair t1 t2, v: V.Pair v1 v2 }
-- nil
eval ρ Nil               = ExplVal { t: ExplNil, v: V.Nil }
-- cons
eval ρ (Cons e e')
 = let ExplVal { t: t1, v: v1 } = eval ρ e
       ExplVal { t: t2, v: v2 } = eval ρ e'
   in  ExplVal { t: ExplCons t1 t2, v: V.Cons v1 v2 }
-- letrec (fun)
eval ρ (Letrec f σ e)     = let ExplVal {t, v} = eval (ρ :+: Bind f (V.Closure ρ f σ)) e
                                t'     = ExplLetrec f (ExplFun ρ σ) t
                                 in ExplVal {t: t', v}
-- apply
eval ρ (App e e')
 = case eval ρ e  of
     ExplVal { t, v: V.Closure ρ' fun σ }
        -> let ExplVal { t: t',  v } = eval ρ e'
           in case match v σ of
                Just (T3 ρ'' e'' m) -> let ExplVal { t: u, v: v' } = eval (conc ρ' ρ'' :+: Bind fun (V.Closure ρ' fun σ)) e''
                                       in  ExplVal { t: ExplApp t t' m u, v: v' }
                Nothing           -> ExplVal { t: ExplBottom, v: V.Failure "Match not found" }
     _  -> ExplVal { t: ExplBottom, v: V.Failure "Expression does not evaluate to closure" }
-- add
eval ρ (Add e1 e2)
 = let ExplVal { t: t1, v: v1 } = eval ρ e1
       ExplVal { t: t2, v: v2 } = eval ρ e2
   in  case v1, v2 of
          V.Int n1, V.Int n2 -> ExplVal { t: ExplAdd t1 t2, v: V.Int (n1 + n2) }
          _,          _      -> ExplVal { t: ExplBottom, v: V.Failure "Arithmetic type error: e1 or/and e2 do not evaluate to ints" }
-- let
eval ρ (Let x e1 e2)
 = let ExplVal { t: t1, v: v1 } = eval ρ e1
       ρ'  = (ρ :+: Bind x v1)
       ExplVal { t: t2, v: v2 }  = eval ρ' e2
   in  ExplVal {t: ExplLet x t1 t2, v: v2 }
-- match (no rule)
eval ρ (Match e σ)
 = let ExplVal { t: t1, v: v1 } = eval ρ e
   in case match v1 σ of
        Nothing            -> ExplVal { t: ExplBottom, v: V.Failure "Match not found" }
        Just (T3 ρ' e' m)  -> let ExplVal { t: t2, v: v2 } = eval (conc ρ ρ') e'
                              in  ExplVal { t: ExplMatch t1 m t2, v: v2 }
