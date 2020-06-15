module Bwd where

import Prelude hiding (absurd, join)
import Data.List (List(..)) as L
import Data.List (List, (:))
import Data.Map (update)
import Data.Tuple (Tuple(..))
import Primitive (primitives)
import Bindings (Bindings(..), (:+:), (↦), ε, find)
import Expl (Expl(..), Def(..)) as T
import Expl (Expl, Match(..))
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), RecDef(..), Def(..), RecDefs)
import Lattice (Selected, (∨), bot)
import Util (T3(..), (≜), type (×), absurd, error, successful)
import Val (Env, Val(..), BinaryOp(..), UnaryOp(..))
import Val (RawVal(..)) as V

unmatch :: Env -> Match -> Env × Env
unmatch (ρ :+: x ↦ v) (MatchVar x')
   = if x == x' then Tuple ρ (ε :+: x ↦ v)
     else error "unmatch - variables do not match"
unmatch Empty (MatchVar x')
   = error "unmatch - variable not found in empty env"
unmatch ρ (MatchConstr (Tuple _ ξs) _) = unmatches ρ ξs

unmatches :: Env -> List Match -> Env × Env
unmatches ρ L.Nil = Tuple ρ ε
unmatches ρ (ξ : ξs) =
   let Tuple ρ' ρ2   = unmatch ρ ξ
       Tuple ρ'' ρ1  = unmatches ρ' ξs in
   Tuple ρ'' (ρ1 <> ρ2)

closeDefs_bwd :: Env -> T3 Env RecDefs Selected
closeDefs_bwd ρ =
   case ρ of
      xs :+: f ↦ v@(Val α_f (V.Closure ρ_f δ_f σ_f))
         -> joinδClsre (foldClosures joinRecDefs (Tuple δ_f v) xs)
      xs :+: x ↦ v
         -> error "non-closure found in env during closeDefs_bwd"
      Empty  -> T3 ε L.Nil false
   where
      joinδClsre (Tuple δ v)
         = case v of Val α_f (V.Closure ρ_f δ_f σ_f) -> T3 ρ_f (δ ∨ δ_f) α_f
                     _                               -> error "not a closure"
      joinRecDefs fσ (Tuple δ clsre)
         = case fσ of (f ↦ x@(Val α_f (V.Closure ρ_f δ_f σ_f)))
                        -> let clsre' = x ∨ clsre
                               δ'     = RecDef f σ_f : δ
                           in  Tuple δ' clsre'
                      _ -> error "not a closure"
      foldClosures f z (xs :+: x ↦ v) = f (x ↦ v) (foldClosures f z xs)
      foldClosures f z Empty          = z

filterRecDefs :: Env -> RecDefs -> Env × Env
filterRecDefs = go ε
   where
   go acc ρ L.Nil = Tuple ρ acc
   go acc (ρ :+: x ↦ v) (RecDef f σ : δ)
      = if f == x then go (acc :+: x ↦ v) ρ δ
        else error "filterRecDefs - function name does not match"
   go acc Empty (RecDef f σ : δ)
      = error "more recdefs than found in environment"

match_bwd :: Env -> Cont -> Selected -> Match -> Val × Elim
match_bwd (ε :+: x ↦ v) κ α (MatchVar x')      = Tuple v (ElimVar (x ≜ x') κ)
match_bwd _ _ _ (MatchVar x')                  = error absurd
match_bwd ρ κ α (MatchConstr (Tuple c ξs) κs)  =
   let Tuple vs κ = matchArgs_bwd ρ κ α ξs in
   Tuple (Val α $ V.Constr c vs) (ElimConstr $ update (const $ pure κ) c $ map bot κs)

matchArgs_bwd :: Env -> Cont -> Selected -> List Match -> List Val × Cont
matchArgs_bwd ρ κ α L.Nil     = Tuple L.Nil κ
matchArgs_bwd ρ κ α (ξ : ξs)  =
   let Tuple ρ' ρ1   = unmatch ρ ξ
       Tuple vs κ'   = matchArgs_bwd ρ' κ α ξs
       Tuple v σ     = match_bwd ρ1 κ' α ξ in
   Tuple (v : vs) $ CElim σ

eval_bwd :: Val -> Expl -> T3 Env Expr Selected
-- true
eval_bwd (Val α V.True ) T.True = T3 ε (Expr α True) α
-- false
eval_bwd (Val α V.False) T.False = T3 ε (Expr α False) α
-- pair
eval_bwd (Val α (V.Pair v1 v2)) (T.Pair t1 t2)
   = let T3 ρ1  e1  α1 = eval_bwd v1 t1
         T3 ρ2  e2  α2 = eval_bwd v2 t2
     in  T3 (ρ1 ∨ ρ2) (Expr α (Pair e1 e2)) (α ∨ α1 ∨ α2)
-- var
eval_bwd (Val α v) (T.Var x) =
   T3 (ε :+: x ↦ (Val α v)) (Expr α (Var x)) false
-- int
eval_bwd (Val α (V.Int n)) (T.Int tn) = T3 ε (Expr α (Int n)) α
-- op
eval_bwd (Val α (V.Binary (BinaryOp s bin))) (T.Op op)
   = T3 (ε :+: op ↦ (Val α (V.Binary (BinaryOp s bin)))) (Expr α (Op op)) false
eval_bwd (Val α (V.Unary (UnaryOp s una))) (T.Op op)
   = T3 (ε :+: op ↦ (Val α (V.Unary (UnaryOp s una)))) (Expr α (Op op)) false
-- nil
eval_bwd (Val α V.Nil) T.Nil = T3 ε (Expr α Nil) α
-- cons
eval_bwd (Val α (V.Cons u v)) (T.Cons t t')
   = let T3 ρ  e  α'  = eval_bwd u t
         T3 ρ' e' α'' = eval_bwd v t'
     in  T3 (ρ ∨ ρ') (Expr α (Cons e e')) (α ∨ α' ∨ α'')
-- lambda
eval_bwd (Val α (V.Closure ρ δ σ)) (T.Lambda σ')
   = T3 ρ (Expr α (Lambda σ)) α
-- apply
eval_bwd v (T.App t t' ξ t'')
   = case eval_bwd v t'' of
      T3 (ρ1ρ2ρ3 :+: f ↦ Val _ (V.Closure ρ1' δ σ)) e α ->
         let Tuple ρ1ρ2 ρ3      = unmatch ρ1ρ2ρ3 ξ
             Tuple ρ1 ρ2        = filterRecDefs ρ1ρ2 δ
             Tuple v' σ         = match_bwd ρ3 (CExpr e) α ξ
             T3 ρ'  e'  α'      = eval_bwd v' t' --
             T3 ρ1' δ   α2      = closeDefs_bwd ρ2
             T3 ρ'' e'' α''     = eval_bwd (Val (α ∨ α2) (V.Closure (ρ1 ∨ ρ1') δ σ)) t
         in  T3 (ρ' ∨ ρ'') (e' ∨ e'') (α' ∨ α'')
      _ -> error absurd
-- binary-apply
eval_bwd (Val α v) (T.BinaryApp t op t')
   = T3 (ρ ∨ ρ') (Expr α (BinaryApp e op e')) α
   where
      f expl = case expl of T.Int n -> eval_bwd (Val α (V.Int n)) expl
                        --  T.Var x -> eval_bwd (Val α (V.Var x)) expl
                            _       -> error ""
      T3 ρ  e  α'  = f t
      T3 ρ' e' α'' = f t'
-- apply-prim
eval_bwd (Val α v) (T.AppOp t t')
   = case t of
      T.Op op -> let val_t = successful (find op primitives)
                     val_t' = case t' of  T.Int i -> Val α (V.Int i)
                                 --     T.Var x -> Val α (V.Var x)
                                          _       -> error absurd
                     T3 ρ  e  α'  = eval_bwd val_t t
                     T3 ρ' e' α'' = eval_bwd val_t' t'
                 in  T3 (ρ ∨ ρ') (e ∨ e') α
      _ -> error absurd
-- let
eval_bwd v (T.Let (T.Def ξ t) t')
   = let T3 ρρ'  e  α   = eval_bwd v  t'
         Tuple ρ ρ'     = unmatch ρρ' ξ
         Tuple v' σ     = match_bwd ρ' (CExpr e) α ξ
         T3 ρ'' e' α'   = eval_bwd v' t
     in  T3 (ρ ∨ ρ'') (Expr (α ∨ α') (Let (Def σ e) e')) (α ∨ α')
-- -- let-rec
eval_bwd v (T.LetRec δ t)
   = let T3 ρ_ρ' e α = eval_bwd v t
         Tuple ρ ρ'  = filterRecDefs ρ_ρ' δ
         T3 _ δ' α'  = closeDefs_bwd ρ'
     in  T3 (ρ ∨ ρ') (Expr false (LetRec δ' e)) (α ∨ α')
eval_bwd _ _ = error absurd
