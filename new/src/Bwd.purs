module Bwd where

import Prelude hiding (absurd, join)
import Data.Foldable (foldr)
import Data.List (List, (:), length, zip, difference, drop)
import Data.List (List(..)) as L
import Data.Map (update)
import Data.Maybe
import Debug.Trace (trace) as T
import Text.Pretty (text)
import Bindings (Bind, Bindings(..), (:+:), (:++:), (◃), (↦), find, foldBind)
import Expl (Expl, Match(..))
import Expl (Expl(..), Def(..)) as T
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), RecDef(..), Def(..), RecDefs)
import Lattice (Selected, class Lattice, (∨?), bot, ff, tt)
import Primitive (primitives, class FromList, fromList, toList, class ToList)
import Pretty
import Util ((≜), type (×), (×), absurd, error, successful)
import Val (Env, Val(..), BinaryOp(..), UnaryOp(..))
import Val (RawVal(..)) as V


trace s a = T.trace (pretty s) $ \_-> a
trace' s a = T.trace  s $ \_-> a


join :: forall a . Pretty a => Lattice a => a -> a -> a
join p q = case p ∨? q of Just a -> a
                          Nothing -> error $ "Join undefined between " <> render (pretty p) <> " and " <> render (pretty q)
infixl 6 join as ∨

unmatch :: Env -> Match -> Env × Env
unmatch (ρ :+: x ↦ v) (MatchVar x')
   = ρ × (Empty :+: (x ≜ x') ↦ v)
unmatch Empty (MatchVar x')
   = error "unmatch - variable not found in empty env"
unmatch ρ (MatchConstr (_ × ξs) _) = unmatches ρ ξs

unmatches :: Env -> List Match -> Env × Env
unmatches ρ L.Nil = ρ × Empty
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
closeDefs_bwd Empty            = Empty × L.Nil × false

split :: Env -> RecDefs -> Env × Env
split = go Empty
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
   (v : vs) × (Arg (length vs) σ)

eval_bwd :: Val -> Expl -> Env × Expr × Selected
-- var
eval_bwd v (T.Var x ρ)
   = ((bot ρ) ◃ (x ↦ v)) × (Expr ff (Var x)) × ff
-- int
eval_bwd (Val α (V.Int n)) (T.Int tn ρ)
   = bot ρ × (Expr ff (Int n)) × ff
-- op
eval_bwd v (T.Op op ρ)
   = (bot ρ ◃ op ↦ v) × (Expr ff (Op op)) × ff
-- lambda
eval_bwd (Val α (V.Closure ρ _ _)) (T.Lambda σ)
   = ρ × (Expr α (Lambda σ)) × α
-- apply
eval_bwd v'' (T.App (t × v@(Val _ (V.Closure ρ1 δ σ))) t' ξ t'')
   =  let ρ1ρ2ρ3 × e × α  = eval_bwd v'' t''

          ρ1ρ2 × ρ3        = unmatch ρ1ρ2ρ3 ξ
          v'   × σ'        = match_bwd ρ3 (Body e) α ξ
          _ × ρ2           = split ρ1ρ2 δ -- don't have access to δ!! need to work on this.
          ρ'  × e'  × α'   = eval_bwd v' t'
         --  kk = trace ξ $ 56
         --  kk' = trace σ' 56
          ρ1' × δ'   × α2  = closeDefs_bwd ρ2
         --  k' = trace' "p1':" $ trace ρ1' 5
          ρ'' × e'' × α'' = eval_bwd (Val (α ∨ α2) (V.Closure (ρ1 ∨ ρ1') δ' σ')) t
         --  k' = trace' "p':" $ trace ρ' $ trace' "p''" $ trace ρ'' $ 5
      in  (ρ' ∨ ρ'') × (Expr ff (App e'' e')) × (α' ∨ α'')
-- binary-apply
eval_bwd (Val α v) (T.BinaryApp (t1 × v1) op (t2 × v2))
   = let ρ  × e  × α'  = eval_bwd v2 t2
         ρ' × e' × α'' = eval_bwd v1 t1
     in  (ρ ∨ ρ') × (Expr α (BinaryApp e' op e)) × ff
-- apply-prim
eval_bwd (Val α v) (T.AppOp (t1 × v1) (t2 × v2))
   = let ρ  × e  × α'  = eval_bwd v2 t2
         ρ' × e' × α'' = eval_bwd v1 t1
     in  (ρ ∨ ρ') × (Expr α (App e e')) × α
-- match-as
eval_bwd v (T.MatchAs t1 ξ t2)
   = let ρ1ρ2 × e × α = eval_bwd v t2
         ρ1 × ρ2      = unmatch ρ1ρ2 ξ
         v' × σ       = match_bwd ρ2 (Body e) α ξ
         ρ1' × e' × α'  = eval_bwd v' t1
         -- k = trace (ρ1' ∨ ρ1) 5
     in  (ρ1' ∨ ρ1) × (Expr ff (MatchAs e' σ)) × (α ∨ α')
-- let
eval_bwd v (T.Let (T.Def ξ t1) t2)
   = let ρ1ρ2 × e2 × α2 = eval_bwd v t2

         ρ1 × ρ2        = unmatch ρ1ρ2 ξ
         v' × σ         = match_bwd ρ2 (Body e2) α2 ξ
         ρ1' × e1 × α1  = eval_bwd v' t1

     in  (ρ1 ∨ ρ1') × (Expr (α1 ∨ α2) (Let (Def σ e1) e2)) × (α1 ∨ α2)
-- let-rec
eval_bwd v (T.LetRec δ t)
   = let ρ1ρ2 × e × α = eval_bwd v t
         _ × ρ2       = split ρ1ρ2 δ
         ρ1 × δ' × α' = closeDefs_bwd ρ2
     in  (ρ1 ∨ ρ2) × (Expr ff (LetRec δ' e)) × (α ∨ α')
-- constr
eval_bwd (Val _ (V.Constr c vs)) (T.Constr c' ts)
   = let f = (\(v × t) (ρ × es × α)
                 -> let ρ' × e × α' = eval_bwd v t
                    in  (ρ ∨ ρ') × (e:es) × (α ∨ α'))
         ρ × es × α' = foldr f (Empty × L.Nil × ff) (zip vs ts)
     in  ρ × (Expr ff (Constr c es)) × α'

eval_bwd v t = error $ "No pattern match found for eval_bwd in \n" <> render (pretty t)
