module Bwd where

import Prelude hiding (absurd, join)
import Bindings (Bindings(..), (:+:), (↦), ε, find, remove, tail)
import Elim (Elim(..))
import Data.List (List(..)) as L
import Data.List ((:))
import Expr (Expr(..), RawExpr(..), RecDef(..), RecDefs)
import Lattice (class Selectable, Selected, (∨), bot, join)
import Util (T3(..), absurd, error, successful, (≜))
import Val (Env, Val(..), BinaryOp(..), UnaryOp(..))
import Val (RawVal(..)) as V
import Expl (Expl(..)) as T
import Expl (Expl, Match(..))
import Data.Tuple (Tuple(..))

unmatch :: forall k . Env -> Match k -> Tuple Env Env
unmatch ρ (MatchVar x)
   =  let Tuple v ρ' = successful (remove x ρ)
      in  Tuple ρ' (ε :+: x ↦ v)
unmatch ρ (MatchTrue k)    = Tuple ρ ε
unmatch ρ (MatchFalse k)   = Tuple ρ ε
unmatch ρ (MatchPair ξ ξ')
   =  let Tuple ρ'  ρ2 = unmatch ρ  ξ'
          Tuple ρ'' ρ1 = unmatch ρ' ξ
      in  Tuple ρ (ρ1 <> ρ2)
unmatch ρ (MatchNil k)     = Tuple ρ ε
unmatch ρ (MatchCons {nil: k, cons: Tuple ξ ξ'})
   =  let Tuple ρ'  ρ2 = unmatch ρ  ξ'
          Tuple ρ'' ρ1 = unmatch ρ' ξ
      in  Tuple ρ (ρ1 <> ρ2)

bound_vars :: forall k . Env -> Match k -> Env
bound_vars ρ (MatchVar x)     = ε :+: x ↦ successful (find x ρ)
bound_vars ρ (MatchTrue k)    = ε
bound_vars ρ (MatchFalse k)   = ε
bound_vars ρ (MatchPair ξ ξ') = append (bound_vars ρ ξ) (bound_vars ρ ξ')
bound_vars ρ (MatchNil k)     = ε
bound_vars ρ (MatchCons {nil: k, cons: Tuple ξ ξ'}) = append (bound_vars ρ ξ) (bound_vars ρ ξ')

joinClosures :: Env -> T3 Env RecDefs Selected
joinClosures ρ =
   case filter ρ isClosure of
      xs :+: f ↦ v@(Val α (V.Closure ρ_f δ_f σ_f)) -> closureToT3 (foldClosures join v xs)
      _ -> error "no closures found in ρ"
   where
      closureToT3 v
         = case v of
            Val α_f (V.Closure ρ_f δ_f σ_f) -> T3 ρ_f δ_f α_f
            _                               -> error "not a closure"

      isClosure v
         = case v of
            Val α_f (V.Closure ρ_f δ_f σ_f) -> true
            _                               -> false
      filter ρ b
         = case ρ of
            (xs :+: f ↦ v) -> if b v then filter xs b :+: f ↦ v else filter xs b
            Empty          -> Empty
      foldClosures f z (xs :+: x ↦ v) = f v (foldClosures f z xs)
      foldClosures f z Empty          = z

filterRecDefs :: Env -> RecDefs -> Tuple Env Env
filterRecDefs = go ε
   where
   go acc ρ L.Nil            = Tuple ρ acc
   go acc ρ (RecDef f σ : δ) = let Tuple v ρ' = successful (remove f ρ)
                               in  go (acc :+: f ↦ v) ρ' δ

match_bwd :: forall k . Selectable k => Env -> k -> Selected -> Match k -> Tuple Val (Elim k)
-- var
match_bwd (ε :+: x ↦ v) κ α (MatchVar x') = Tuple v (ElimVar (x ≜ x') κ)
-- true
match_bwd ε κ α (MatchTrue κ')  = Tuple (Val α V.True) (ElimBool { true: κ, false: bot κ' })
-- false
match_bwd ε κ α (MatchFalse κ') = Tuple (Val α V.False) (ElimBool { true: bot κ', false: κ })
-- pair
match_bwd ρ κ α (MatchPair ξ ξ') =
   let ρ1 = bound_vars ρ ξ
       ρ2 = bound_vars ρ ξ'
       Tuple v' σ  = match_bwd ρ2 κ α ξ'
       Tuple v  τ  = match_bwd ρ1 σ α ξ
   in  Tuple (Val α (V.Pair v v')) (ElimPair τ)
-- nil
match_bwd ε κ α (MatchNil σ) = Tuple (Val α V.Nil) (ElimList {nil: κ, cons: bot σ})
-- cons
match_bwd ρ κ α (MatchCons { nil: κ', cons: Tuple ξ ξ'}) =
   let ρ1 = bound_vars ρ ξ
       ρ2 = bound_vars ρ ξ'
       Tuple v' σ  = match_bwd ρ2 κ α ξ'
       Tuple v  τ  = match_bwd ρ1 σ α ξ
   in  Tuple (Val α (V.Cons v v')) (ElimList {nil: bot κ, cons: τ})
match_bwd _ _ _ _ = error absurd

eval_bwd :: Val -> Expl -> T3 Env Expr Selected
-- true
eval_bwd (Val α V.True ) T.True = T3 ε (Expr α True) α
-- false
eval_bwd (Val α V.False) T.False = T3 ε (Expr α False) α
-- pair
eval_bwd (Val α (V.Pair v1 v2)) (T.Pair t1 t2)
   = let T3 ρ1  e1  α1 = eval_bwd v1 t1
         T3 ρ2  e2  α2 = eval_bwd v2 t2
     in  T3 (join ρ1 ρ2) (Expr α (Pair e1 e2)) (α ∨ α1 ∨ α2)
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
eval_bwd (Val α (V.Cons u v)) (T.Cons tT uU)
   = let T3 ρ  e  α'  = eval_bwd u tT
         T3 ρ' e' α'' = eval_bwd v uU
     in  T3 (join ρ ρ') (Expr α (Cons e e')) (α ∨ α' ∨ α'')
-- apply
eval_bwd v (T.App t u ξ t')
   = case eval_bwd v t' of
      T3 (ρ1ρ2ρ3 :+: f ↦ Val _ (V.Closure ρ1' δ σ)) e α ->
         let Tuple ρ1ρ2 ρ3      = unmatch ρ1ρ2ρ3 ξ
             Tuple ρ1 ρ2        = filterRecDefs ρ1ρ2 δ
             Tuple v' σ         = match_bwd ρ3 e α ξ
             T3 ρ'  e'  α'      = eval_bwd v' u
             T3 p1' δ   α2      = joinClosures ρ2
             T3 ρ'' e'' α''     = eval_bwd (Val (α ∨ α') (V.Closure (ρ1 ∨ ρ1') δ σ)) t
         in  T3 (ρ' ∨ ρ'') (e' ∨ e'') (α' ∨ α'')
      _ -> error absurd
-- -- binary-apply
-- eval_bwd (Val α (V.Int n)) (T.BinaryApp t op u)
--    = case t, u of
--       (T.Int val_t), (T.Int val_u) ->
--          let T3 ρ  e  α'  = eval_bwd (Val α (V.Int val_t)) t
--              T3 ρ' e' α'' = eval_bwd (Val α (V.Int val_u)) u
--          in  T3 (ρ ∨ ρ') (Expr α (BinaryApp e e')) α
--       _, _ -> error absurd
-- -- apply-prim
-- eval_bwd (Val α (V.Int n)) (T.AppOp t u)
--    = case t of
--       (T.Op op) -> let val_t = successful (find op primitives)
--                        T3 ρ e α' = eval_bwd val_t t
--                    in  case u of
--                         (T.Int i) ->
--                            let val_u = Val α i
--                                T3 ρ' e' α'' = eval_bwd val_u u
--                            in  T3 (ρ ∨ ρ') (e ∨ e') (α)
--                         (T.Var x) ->
--                         _
--       _ -> error absurd
-- let
-- eval_bwd val (T.Let x t1 t2) = ...
-- -- let-rec
-- eval_bwd val (T.Letrec x t1 t2) = ...
eval_bwd _ _ = error absurd