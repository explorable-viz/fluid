module Bwd where

import Prelude hiding (absurd, join)
import Data.Foldable (foldr)
import Data.List (List, (:), zip)
import Data.List (List(..)) as L
import Data.Map (update)
import Bindings (Bind, Bindings(..), (:+:), (↦), ε, find, foldBind)
import Expl (Expl, Match(..))
import Expl (Expl(..), Def(..)) as T
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), RecDef(..), Def(..), RecDefs)
import Lattice (Selected, (∨), bot)
import Primitive (primitives)
import Util ((≜), type (×), (×), absurd, error, successful)
import Val (Env, Val(..), BinaryOp(..), UnaryOp(..))
import Val (RawVal(..)) as V

unmatch :: Env -> Match -> Env × Env
unmatch (ρ :+: x ↦ v) (MatchVar x')
   = ρ × (ε :+: (x ≜ x') ↦ v)
unmatch Empty (MatchVar x')
   = error "unmatch - variable not found in empty env"
unmatch ρ (MatchConstr (_ × ξs) _) = unmatches ρ ξs

unmatches :: Env -> List Match -> Env × Env
unmatches ρ L.Nil = ρ × ε
unmatches ρ (ξ : ξs) =
   let ρ'  × ρ2   = unmatch ρ ξ
       ρ'' × ρ1   = unmatches ρ' ξs in
   ρ'' × (ρ1 <> ρ2)


joinδ :: RecDefs × (Env × RecDefs × Selected) -> Env × RecDefs × Selected
joinδ (δ' × ρ × δ × α) = ρ × (δ ∨ δ') × α

closeDefs_bwd :: Env -> Env × RecDefs × Selected
closeDefs_bwd (ρ' :+: f0 ↦ Val α0 (V.Closure ρ0 δ0 σ0))
   = joinδ $ foldBind joinClsre ((RecDef f0 σ0 : L.Nil) × ρ0 × δ0 × α0) ρ'
   where
      joinClsre   :: Bind Val
                  -> RecDefs × (Env × RecDefs × Selected)
                  -> RecDefs × (Env × RecDefs × Selected)
      joinClsre (f ↦ Val α_f (V.Closure ρ_f δ_f σ_f)) (δ_acc × ρ × δ × α)
         = (RecDef f σ_f : δ_acc) × (ρ ∨ ρ_f) × (δ ∨ δ_f) × (α ∨ α_f)
      joinClsre (_ ↦ _) _      = error absurd

closeDefs_bwd (_  :+: _ ↦ _)   = error absurd
closeDefs_bwd Empty            = ε × L.Nil × false

split :: Env -> RecDefs -> Env × Env
split = go ε
   where
   go acc ρ L.Nil                         = ρ × acc
   go acc (ρ :+: x ↦ v) (RecDef f σ : δ)  = go (acc :+: (x ≜ f) ↦ v) ρ δ
   go acc Empty _                         = error absurd

match_bwd :: Env -> Cont -> Selected -> Match -> Val × Elim
match_bwd (Empty :+: x ↦ v) κ α (MatchVar x')  = v × (ElimVar (x ≜ x') κ)
match_bwd _ _ _ (MatchVar x')                  = error absurd
match_bwd ρ κ α (MatchConstr (c × ξs) κs)  =
   let vs × κ = matchArgs_bwd ρ κ α ξs in
   (Val α $ V.Constr c vs) × (ElimConstr $ update (const $ pure κ) c $ map bot κs)

matchArgs_bwd :: Env -> Cont -> Selected -> List Match -> List Val × Cont
matchArgs_bwd ρ κ α L.Nil     = L.Nil × κ
matchArgs_bwd ρ κ α (ξ : ξs)  =
   let ρ' × ρ1   = unmatch ρ ξ
       vs × κ'   = matchArgs_bwd ρ' κ α ξs
       v  × σ    = match_bwd ρ1 κ' α ξ in
   (v : vs) × (CElim σ)

eval_bwd :: Val -> Expl -> Env × Expr × Selected
-- true
eval_bwd (Val α V.True ) T.True = ε × (Expr α True) × α
-- false
eval_bwd (Val α V.False) T.False = ε × (Expr α False) × α
-- pair
eval_bwd (Val α (V.Pair v1 v2)) (T.Pair t1 t2)
   =  let ρ1 × e1 × α1 = eval_bwd v1 t1
          ρ2 × e2 × α2 = eval_bwd v2 t2 in
      (ρ1 ∨ ρ2) × (Expr α (Pair e1 e2)) × (α ∨ α1 ∨ α2)
-- var
eval_bwd (Val α v) (T.Var x)
   = (ε :+: x ↦ Val α v) × (Expr α (Var x)) × false
-- int
eval_bwd (Val α (V.Int n)) (T.Int tn)
   = ε × (Expr α (Int n)) × α
-- op
eval_bwd (Val α (V.Binary (BinaryOp s bin))) (T.Op op)
   = (ε :+: op ↦ (Val α (V.Binary (BinaryOp s bin)))) × (Expr α (Op op)) × false
eval_bwd (Val α (V.Unary (UnaryOp s una))) (T.Op op)
   = (ε :+: op ↦ (Val α (V.Unary (UnaryOp s una)))) × (Expr α (Op op)) × false
-- nil
eval_bwd (Val α V.Nil) T.Nil = ε × (Expr α Nil) × α
-- cons
eval_bwd (Val α (V.Cons u v)) (T.Cons t t')
   = let ρ  × e  × α'  = eval_bwd u t
         ρ' × e' × α'' = eval_bwd v t' in
     (ρ ∨ ρ') × (Expr α (Cons e e')) × (α ∨ α' ∨ α'')
-- lambda
eval_bwd (Val α (V.Closure ρ δ σ)) (T.Lambda σ')
   = ρ × (Expr α (Lambda σ)) × α
-- apply
eval_bwd v@(Val _ (V.Closure _ δ _)) (T.App t t' ξ t'')
   =  let ρ1ρ2ρ3 × e × α  = eval_bwd v t''
          ρ1ρ2 × ρ3       = unmatch ρ1ρ2ρ3 ξ
          ρ1   × ρ2       = split ρ1ρ2 δ
          v'   × σ        = match_bwd ρ3 (CExpr e) α ξ
          ρ'  × e'  × α'  = eval_bwd v' t'
          ρ1' × δ   × α2  = closeDefs_bwd ρ2
          ρ'' × e'' × α'' = eval_bwd (Val (α ∨ α2) (V.Closure (ρ1 ∨ ρ1') δ σ)) t in
      (ρ' ∨ ρ'') × (Expr false (App e'' e')) × (α' ∨ α'')
-- binary-apply
eval_bwd (Val α v) (T.BinaryApp t op t')
   = (ρ ∨ ρ') × (Expr α (BinaryApp e op e')) × α
   where
      f expl = case expl of T.Int n -> eval_bwd (Val α (V.Int n)) expl
                        --  T.Var x -> eval_bwd (Val α (V.Var x)) expl
                            _       -> error ""
      ρ  × e  × α'  = f t
      ρ' × e' × α'' = f t'
-- apply-prim
eval_bwd (Val α v) (T.AppOp t t')
   = case t of
      T.Op op -> let val_t  = successful (find op primitives)
                     val_t' = case t' of  T.Int i -> Val α (V.Int i)
                                 --     T.Var x -> Val α (V.Var x)
                                          _       -> error absurd
                     ρ  × e  × α'  = eval_bwd val_t t
                     ρ' × e' × α'' = eval_bwd val_t' t'
                 in  (ρ ∨ ρ') × (Expr false (App e e')) × α
      _ -> error absurd
-- let
eval_bwd v (T.Let (T.Def ξ t) t')
   = let ρρ' ×  e × α   = eval_bwd v  t'
         ρ  × ρ'        = unmatch ρρ' ξ
         v' × σ         = match_bwd ρ' (CExpr e) α ξ
         ρ'' × e' × α'  = eval_bwd v' t
     in  (ρ ∨ ρ'') × (Expr (α ∨ α') (Let (Def σ e) e')) × (α ∨ α')
-- let-rec
eval_bwd v (T.LetRec δ t)
   = let ρ_ρ' × e × α = eval_bwd v t
         ρ × ρ'       = split ρ_ρ' δ
         _ × δ' × α'  = closeDefs_bwd ρ'
     in  (ρ ∨ ρ') × (Expr false (LetRec δ' e)) × (α ∨ α')
-- constr
eval_bwd (Val _ (V.Constr c vs)) (T.Constr c' ts)
   = let f = (\(v × t) (ρ × es × α)
                 -> let ρ' × e × α' = eval_bwd v t
                    in  (ρ ∨ ρ') × (e:es) × (α ∨ α'))
         ρ × es × α' = foldr f (ε × L.Nil × false) (zip vs ts)
     in  ρ × (Expr false (Constr c es)) × α'
eval_bwd _ _ = error absurd
