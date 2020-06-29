module Bwd where

import Prelude hiding (absurd, join)
import Data.Foldable (foldr, foldl)
import Data.List (List, (:), zip, zipWith, difference, reverse, drop)
import Data.List (List(..), tail, head, length) as L
import Data.Map (update, insert)
import Data.Maybe
import Debug.Trace (trace) as T
import Text.Pretty (text)
import Bindings (Bind, Bindings(..), (:+:), (:++:), (◃), (↦), find, head, foldBind, length)
import DataType (Ctr(..))
import Expl (Expl, Match(..))
import Expl (Expl(..), VarDef(..)) as T
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), RecDef(..), VarDef(..), RecDefs)
import Lattice (Selected, class Lattice, (∨?), bot, ff, tt)
import Primitive (primitives, class FromList, fromList, toList, class ToList)
import Pretty
import Util ((≟), type (×), (×), fromJust, absurd, error, successful)
import Val (Env, Val(..), BinaryOp(..), UnaryOp(..))
import Val (RawVal(..)) as V


trace s a = T.trace (pretty s) $ \_-> a
trace' s a = T.trace  s $ \_-> a

-- mustEq :: forall a . Eq a => a -> a -> a
mustEq x x' = fromJust ("Must be equal: " <> x <> " " <> x') $ x ≟ x'
infixl 5 mustEq as ≜

join :: forall a . Pretty a => Lattice a => a -> a -> a
join p q = case p ∨? q of Just a -> a
                          Nothing -> error $ "Join undefined between \n" <> render (pretty p) <> " and \n" <> render (pretty q)
infixl 6 join as ∨

unmatch :: Env -> Match -> Env × Env
unmatch ρ (MatchVar x')
   = unmatchOne ρ (MatchVar x')
unmatch ρ (MatchConstr (ctr × ξs) m) =
   unmatchOne ρ (MatchConstr (ctr × (reverse ξs)) m)

unmatchOne :: Env -> Match -> Env × Env
unmatchOne (ρ :+: x ↦ v) (MatchVar x')
   = ρ × (Empty :+: (x ≜ x') ↦ v)
unmatchOne Empty (MatchVar x')
   = error "unmatch - variable not found in empty env"
unmatchOne ρ (MatchConstr (_ × ξs) _) =
   unmatchMany ρ ξs

unmatchMany :: Env -> List Match -> Env × Env
unmatchMany ρ L.Nil = ρ × Empty
unmatchMany ρ (ξ : ξs) =
   let ρ'  × ρ2   = unmatchOne ρ ξ
       ρ'' × ρ1   = unmatchMany ρ' ξs in
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
match_bwd ρ κ α (MatchVar x')
   = matchOne_bwd ρ κ α (MatchVar x')
match_bwd ρ κ α (MatchConstr (c × ξs) κs)
   = matchOne_bwd ρ κ α (MatchConstr (c × (reverse ξs)) κs)


matchOne_bwd :: Env -> Cont -> Selected -> Match -> Val × Elim
matchOne_bwd (Empty :+: x ↦ v) κ α (MatchVar x')  = v × (ElimVar (x ≜ x') κ)
matchOne_bwd _ _ _ (MatchVar x')                  = error absurd
matchOne_bwd ρ κ α (MatchConstr (c × ξs) κs)  =
   let vs × κ' = matchMany_bwd ρ κ α ξs
   in (Val α $ V.Constr c vs) × (ElimConstr $ insert c κ' $ map bot κs)

matchMany_bwd :: Env -> Cont -> Selected -> List Match -> List Val × Cont
matchMany_bwd ρ κ α L.Nil     = L.Nil × κ
matchMany_bwd ρ κ α (ξ : ξs)  =
   let ρ' × ρ1   = unmatch ρ ξ
       v  × σ    = matchOne_bwd ρ1 κ α ξ
       vs × κ'   = matchMany_bwd ρ' (Arg σ) α ξs
   in  (vs <> L.Cons v L.Nil) × κ'

eval_bwd :: Val -> Expl -> Env × Expr × Selected
-- var
eval_bwd v (T.Var x ρ)
   = (bot ρ ◃ x ↦ v) × (Expr ff (Var x)) × ff
-- int
eval_bwd (Val α (V.Int n)) (T.Int tn ρ)
   = bot ρ × (Expr α (Int n)) × α
-- op
eval_bwd v (T.Op op ρ)
   = (bot ρ ◃ op ↦ v) × (Expr ff (Op op)) × ff
-- lambda
eval_bwd (Val α (V.Closure ρ _ _)) (T.Lambda σ)
   = ρ × (Expr α (Lambda σ)) × α
-- apply
eval_bwd v'' (T.App (t × v@(Val _ (V.Closure _ δ _))) t' ξ t'')
   = let ρ1ρ2ρ3 × e × α    = eval_bwd v'' t''
         ρ1ρ2 × ρ3         = unmatch ρ1ρ2ρ3 ξ
         v'   × σ          = match_bwd ρ3 (Body e) α ξ
         ρ1 × ρ2           = split ρ1ρ2 δ
         ρ'  × e'  × α'    = eval_bwd v' t'
         ρ1' × δ'   × α2   = closeDefs_bwd ρ2
         ρ'' × e'' × α''  = eval_bwd (Val (α ∨ α2) (V.Closure (ρ1 ∨ ρ1') δ' σ)) t
         k = trace t 5
     in (ρ' ∨ ρ'') × (Expr (α' ∨ α'') (App e'' e')) × (α' ∨ α'')
-- binary-apply
eval_bwd (Val α v) (T.BinaryApp (t1 × v1) op (t2 × v2))
   = let ρ  × e  × α'  = eval_bwd v2 t2
         ρ' × e' × α'' = eval_bwd v1 t1
     in  (ρ ∨ ρ') × (Expr α (BinaryApp e' op e)) × α
-- apply-prim
eval_bwd (Val α v) (T.AppOp (t1 × v1) (t2 × v2))
   = let ρ  × e  × α'  = eval_bwd v2 t2
         ρ' × e' × α'' = eval_bwd v1 t1
     in  (ρ ∨ ρ') × (Expr α (App e e')) × α
-- match-as
eval_bwd v (T.MatchAs t1 ξ t2)
   = let ρ1ρ2 × e × α = eval_bwd v t2
         ρ1 × ρ2 = unmatch ρ1ρ2 ξ
         v1 × σ = match_bwd ρ2 (Body e) α ξ
         ρ1' × e' × α'  = eval_bwd v1 t1

     in  (ρ1' ∨ ρ1) × (Expr (α ∨ α') (MatchAs e' σ)) × (α ∨ α')
-- let
eval_bwd v (T.Let (T.VarDef ξ t1) t2)
   = let ρ1ρ2 × e2 × α2 = eval_bwd v t2
         ρ1 × ρ2        = unmatch ρ1ρ2 ξ
         v' × σ         = match_bwd ρ2 (Body e2) α2 ξ
         ρ1' × e1 × α1  = eval_bwd v' t1

     in  (ρ1 ∨ ρ1') × (Expr (α1 ∨ α2) (Let (VarDef σ e1) e2)) × (α1 ∨ α2)
-- let-rec
eval_bwd v (T.LetRec δ t)
   = let ρ1ρ2 × e × α = eval_bwd v t

         ρ1 × ρ2       = split ρ1ρ2 δ
         ρ1' × δ' × α' = closeDefs_bwd ρ2

     in  (ρ1 ∨ ρ1') × (Expr (α ∨ α') (LetRec δ' e)) × (α ∨ α')
-- constr
eval_bwd (Val α (V.Constr c vs)) (T.Constr c' ts)
   = let
         evalArgs_bwd :: List Val -> List Expl -> Env × List Expr × Boolean
         evalArgs_bwd (v:vs') (t:ts') =
            let ρ  × e  × α   = eval_bwd v t
                ρ' × e' × α'  = evalArgs_bwd vs' ts'
            in  case ρ' of Empty -> ρ × (e:L.Nil) × α
                           _     -> (ρ ∨ ρ') × (e:e') × (α ∨ α')
         evalArgs_bwd L.Nil L.Nil = Empty × L.Nil × ff
         evalArgs_bwd _ _ = error absurd

         ρ  × es  × α'   = evalArgs_bwd vs ts

     in  ρ × (Expr α (Constr c es)) × (α ∨ α')
eval_bwd (Val α (V.Constr c vs)) (T.NullConstr c' ρ)
   = bot ρ × (Expr α (Constr c L.Nil)) × α
eval_bwd v t = error $ "No pattern match found for eval_bwd in \n" <> render (pretty t)

