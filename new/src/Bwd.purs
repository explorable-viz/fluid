module Bwd where

import Prelude (map, ($), (<>))
import Data.List (List, (:), reverse)
import Data.List (List(..)) as L
import Data.Map (insert)
import Expl (Expl, Match, Match'(..))
import Expl (Expl'(..), VarDef'(..)) as T
import Expr (Cont, Cont'(..), Elim, Elim'(..), Expr, Expr'(..), RawExpr(..), RecDef'(..), VarDef'(..), RecDefs, varAnon)
import Lattice (Selected, bot, (∨))
import Pretty (pretty, render)
import Util (type (×), absurd, error, (×), (≜))
import Val (Bind, Env, Env'(..), Val, Val'(..), (:+:), (↦), (◃), foldEnv)
import Val (RawVal'(..)) as V

unmatch :: Env -> Match -> Env × Env
unmatch ρ (MatchVar x)
   = unmatchOne ρ (MatchVar x)
unmatch ρ (MatchVarAnon v)
   = unmatchOne ρ (MatchVarAnon v)
unmatch ρ (MatchConstr (ctr × ξs) m) =
   unmatchOne ρ (MatchConstr (ctr × reverse ξs) m)

unmatchOne :: Env -> Match -> Env × Env
unmatchOne (ρ :+: x ↦ v) (MatchVar x')
   = ρ × (Empty :+: (x ≜ x') ↦ v)
unmatchOne Empty (MatchVar x')
   = error absurd
unmatchOne ρ (MatchVarAnon _)
   = ρ × Empty
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

closeDefs_bwd :: Env -> Env -> Env × RecDefs × Selected
closeDefs_bwd (ρ' :+: f0 ↦ Val α0 (V.Closure ρ0 δ0 σ0)) _
   = joinδ $ foldEnv joinClsre ((RecDef f0 σ0 : L.Nil) × ρ0 × δ0 × α0) ρ'
   where
      joinClsre   :: Bind
                  -> RecDefs × (Env × RecDefs × Selected)
                  -> RecDefs × (Env × RecDefs × Selected)
      joinClsre (f ↦ Val α_f (V.Closure ρ_f δ_f σ_f)) (δ_acc × ρ × δ × α)
         = (RecDef f σ_f : δ_acc) × (ρ ∨ ρ_f) × (δ ∨ δ_f) × (α ∨ α_f)
      joinClsre (_ ↦ _) _      = error absurd
closeDefs_bwd (_  :+: _ ↦ _) _ = error absurd
closeDefs_bwd Empty ρ1         = bot ρ1 × L.Nil × false

split :: Env -> RecDefs -> Env × Env
split = go Empty
   where
   go acc ρ L.Nil                         = ρ × acc
   go acc (ρ :+: x ↦ v) (RecDef f σ : δ)  = go (acc :+: (x ≜ f) ↦ v) ρ δ
   go acc Empty _                         = error absurd

match_bwd :: Env -> Cont -> Selected -> Match -> Val × Elim
match_bwd ρ κ α (MatchVar x)
   = matchOne_bwd ρ κ α (MatchVar x)
match_bwd ρ κ α (MatchVarAnon v)
   = matchOne_bwd ρ κ α (MatchVarAnon v)
match_bwd ρ κ α (MatchConstr (c × ξs) κs)
   = matchOne_bwd ρ κ α (MatchConstr (c × reverse ξs) κs)

matchOne_bwd :: Env -> Cont -> Selected -> Match -> Val × Elim
matchOne_bwd (Empty :+: x ↦ v) κ α (MatchVar x')   = v × ElimVar (x ≜ x') κ
matchOne_bwd Empty κ α (MatchVarAnon v)            = bot v × ElimVar varAnon κ
matchOne_bwd ρ κ α (MatchConstr (c × ξs) κs)       =
   let vs × κ' = matchMany_bwd ρ κ α ξs in
   (Val α $ V.Constr c vs) × (ElimConstr $ insert c κ' $ map bot κs)
matchOne_bwd _ _ _ _                               = error absurd

matchMany_bwd :: Env -> Cont -> Selected -> List Match -> List Val × Cont
matchMany_bwd ρ κ α L.Nil     = L.Nil × κ
matchMany_bwd ρ κ α (ξ : ξs)  =
   let ρ' × ρ1   = unmatch ρ ξ
       v  × σ    = matchOne_bwd ρ1 κ α ξ
       vs × κ'   = matchMany_bwd ρ' (Arg σ) α ξs in
   (vs <> L.Cons v L.Nil) × κ'

eval_bwd :: Val -> Expl -> Env × Expr × Selected
eval_bwd v (T.Var x ρ)
   = (bot ρ ◃ x ↦ v) × (Expr false (Var x)) × false
eval_bwd (Val α (V.Str s)) (T.Str ts ρ)
   = bot ρ × (Expr α (Str s)) × α
eval_bwd (Val α (V.Int n)) (T.Int tn ρ)
   = bot ρ × (Expr α (Int n)) × α
eval_bwd v@(Val α (V.Primitive φ)) (T.Op op ρ)
   = (bot ρ ◃ op ↦ v) × (Expr false (Op op)) × false
eval_bwd (Val α (V.Closure ρ δ σ)) (T.Lambda σ')
   = ρ × (Expr α (Lambda σ)) × α
eval_bwd v'' (T.App (t × v@(Val _ (V.Closure _ δ _))) t' ξ t'')
   = let ρ1ρ2ρ3 × e × α    = eval_bwd v'' t''
         ρ1ρ2 × ρ3         = unmatch ρ1ρ2ρ3 ξ
         v'   × σ          = match_bwd ρ3 (Body e) α ξ
         ρ1 × ρ2           = split ρ1ρ2 δ
         ρ'  × e'  × α'    = eval_bwd v' t'
         ρ1' × δ'   × α2   = closeDefs_bwd ρ2 ρ1
         ρ'' × e'' × α''   = eval_bwd (Val (α ∨ α2) (V.Closure (ρ1 ∨ ρ1') δ' σ)) t in
      (ρ' ∨ ρ'') × (Expr (α' ∨ α'') (App e'' e')) × (α' ∨ α'')
eval_bwd (Val α v) (T.BinaryApp (t1 × v1) op (t2 × v2))
   = let ρ  × e  × α'  = eval_bwd v2 t2
         ρ' × e' × α'' = eval_bwd v1 t1 in
     (ρ ∨ ρ') × (Expr α (BinaryApp e' op e)) × α
eval_bwd (Val α v) (T.AppOp (t1 × v1) (t2 × v2))
   = let ρ  × e  × α'  = eval_bwd v2 t2
         ρ' × e' × α'' = eval_bwd v1 t1 in
     (ρ ∨ ρ') × (Expr α (App e e')) × α
eval_bwd v (T.MatchAs t1 ξ t2)
   = let ρ1ρ2 × e × α   = eval_bwd v t2
         ρ1 × ρ2        = unmatch ρ1ρ2 ξ
         v1 × σ         = match_bwd ρ2 (Body e) α ξ
         ρ1' × e' × α'  = eval_bwd v1 t1 in
     (ρ1' ∨ ρ1) × (Expr (α ∨ α') (MatchAs e' σ)) × (α ∨ α')
eval_bwd v (T.Let (T.VarDef ξ t1) t2)
   = let ρ1ρ2 × e2 × α2 = eval_bwd v t2
         ρ1 × ρ2        = unmatch ρ1ρ2 ξ
         v' × σ         = match_bwd ρ2 (Body e2) α2 ξ
         ρ1' × e1 × α1  = eval_bwd v' t1 in
     (ρ1 ∨ ρ1') × (Expr (α1 ∨ α2) (Let (VarDef σ e1) e2)) × (α1 ∨ α2)
eval_bwd v (T.LetRec δ t)
   = let ρ1ρ2 × e × α   = eval_bwd v t
         ρ1 × ρ2        = split ρ1ρ2 δ
         ρ1' × δ' × α'  = closeDefs_bwd ρ2 ρ1 in
     (ρ1 ∨ ρ1') × (Expr (α ∨ α') (LetRec δ' e)) × (α ∨ α')
eval_bwd (Val α (V.Constr c vs)) (T.Constr c' ts)
   = let
         evalArgs_bwd :: List Val -> List Expl -> Env × List Expr × Boolean
         evalArgs_bwd (v:vs') (t:ts') =
            let ρ  × e  × α   = eval_bwd v t
                ρ' × e' × α'  = evalArgs_bwd vs' ts'
            in  case ρ' of Empty -> ρ × (e:L.Nil) × α
                           _     -> (ρ ∨ ρ') × (e:e') × (α ∨ α')
         evalArgs_bwd L.Nil L.Nil = Empty × L.Nil × false
         evalArgs_bwd _ _ = error absurd

         ρ  × es  × α'   = evalArgs_bwd vs ts in
     ρ × (Expr α (Constr c es)) × (α ∨ α')
eval_bwd (Val α (V.Constr c vs)) (T.NullConstr c' ρ)
   = bot ρ × (Expr α (Constr c L.Nil)) × α
eval_bwd v t = error $ "No pattern match found for eval_bwd in \n" <> render (pretty t)

