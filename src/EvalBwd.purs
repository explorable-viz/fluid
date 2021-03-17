module EvalBwd where

import Prelude hiding (absurd)
import Data.Array (replicate) as A
import Data.List (List(..), (:), foldr, range, reverse, singleton, unsnoc, zip)
import Data.List (length) as L
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (fromFoldable)
import Data.NonEmpty (foldl1)
import Bindings (Binding, Bindings(..), (:+:), (↦), (◃), length, find, foldEnv, splitAt, varAnon)
import DataType (cPair)
import Expl (Expl, Match(..))
import Expl (Expl(..), VarDef(..)) as T
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), RecDefs)
import Lattice (𝔹, (∨), botOf, expand)
import Util (Endo, type (×), (×), (≜), (!), absurd, error, fromJust, nonEmpty, replicate, successful)
import Val (Env, PrimOp(..), Val)
import Val (Val(..)) as V

unmatch :: Env 𝔹 -> Match 𝔹 -> Env 𝔹 × Env 𝔹
unmatch (ρ :+: x ↦ v) (MatchVar x') = ρ × (Empty :+: (x ≜ x') ↦ v)
unmatch Empty (MatchVar x')         = error absurd
unmatch ρ (MatchVarAnon _)          = ρ × Empty
unmatch ρ (MatchConstr _ ws _)      = unmatchArgs ρ (reverse ws)

-- matches are in a reverse order to the original arguments, to correspond with the 'snoc' order of ρ
unmatchArgs :: Env 𝔹 -> List (Match 𝔹) -> Env 𝔹 × Env 𝔹
unmatchArgs ρ Nil       = ρ × Empty
unmatchArgs ρ (w : ws)  = ρ'' × (ρ1 <> ρ2)
   where ρ'  × ρ2 = unmatch ρ w
         ρ'' × ρ1 = unmatchArgs ρ' ws

-- second argument contains original environment and recursive definitions
closeDefsBwd :: Env 𝔹 -> Env 𝔹 × RecDefs 𝔹 -> Env 𝔹 × RecDefs 𝔹 × 𝔹
closeDefsBwd ρ (ρ0 × δ0) =
   case foldEnv joinDefs (Empty × botOf ρ0 × botOf δ0 × false) ρ of
   δ' × ρ' × δ × α -> ρ' × (δ ∨ δ') × α
   where
   joinDefs :: Binding Val 𝔹 -> Endo (RecDefs 𝔹 × Env 𝔹 × RecDefs 𝔹 × 𝔹)
   joinDefs (f ↦ V.Closure ρ_f δ_f σ_f) (δ_acc × ρ' × δ × α)
      = (δ_acc :+: f ↦ σ_f) × (ρ' ∨ ρ_f) × (δ ∨ δ_f) × α
   joinDefs (f ↦ V.Hole) (δ_acc × ρ' × δ × α) = (δ_acc :+: f ↦ botOf (successful $ find f δ0)) × ρ' × δ × α
   joinDefs (_ ↦ _) _ = error absurd

matchBwd :: Env 𝔹 -> Cont 𝔹 -> 𝔹 -> Match 𝔹 -> Val 𝔹 × Elim 𝔹
matchBwd (Empty :+: x ↦ v) κ α (MatchVar x')   = v × ElimVar (x ≜ x') κ
matchBwd Empty κ α (MatchVarAnon v)            = botOf v × ElimVar varAnon κ
matchBwd ρ κ α (MatchConstr c ws cs)            = V.Constr α c vs × ElimConstr (fromFoldable cκs)
   where vs × κ' = matchArgs_bwd ρ κ α (reverse ws)
         cκs = c × κ' : ((_ × ContHole) <$> cs)
matchBwd _ _ _ _                               = error absurd

matchArgs_bwd :: Env 𝔹 -> Cont 𝔹 -> 𝔹 -> List (Match 𝔹) -> List (Val 𝔹) × Cont 𝔹
matchArgs_bwd ρ κ α Nil       = Nil × κ
matchArgs_bwd ρ κ α (w : ws)  =
   let ρ' × ρ1   = unmatch ρ w
       v  × σ    = matchBwd ρ1 κ α w
       vs × κ'   = matchArgs_bwd ρ' (ContElim σ) α ws in
   (vs <> v : Nil) × κ'

evalBwd :: Val 𝔹 -> Expl 𝔹 -> Env 𝔹 × Expr 𝔹 × 𝔹
evalBwd v (T.Var ρ x) =
   (botOf ρ ◃ x ↦ v) × Var x × false
evalBwd v (T.Op ρ op) =
   (botOf ρ ◃ op ↦ v) × Op op × false
evalBwd V.Hole t@(T.Str _ str) =
   evalBwd (V.Str false str) t
evalBwd (V.Str α s) (T.Str ρ s') | s == s' =
   botOf ρ × Str α s × α
evalBwd _ (T.Str _ _) =
   error absurd
evalBwd V.Hole t@(T.Int _ n) =
   evalBwd (V.Int false n) t
evalBwd (V.Int α n) (T.Int ρ n') | n == n' =
   botOf ρ × Int α n × α
evalBwd _ (T.Int _ _) =
   error absurd
evalBwd V.Hole t@(T.Float _ n) =
   evalBwd (V.Float false n) t
evalBwd (V.Float α n) (T.Float ρ n') | n == n' =
   botOf ρ × Float α n × α
evalBwd _ (T.Float _ _) =
   error absurd
evalBwd V.Hole t@(T.Lambda ρ σ) =
   evalBwd (V.Closure (botOf ρ) Empty (botOf σ)) t
evalBwd (V.Closure ρ Empty σ) (T.Lambda _ _) =
   ρ × Lambda σ × false
evalBwd _ (T.Lambda _ _) =
   error absurd
evalBwd V.Hole t@(T.Constr _ c ts) =
   evalBwd (V.Constr false c (ts <#> const V.Hole)) t
evalBwd (V.Constr α c vs) (T.Constr ρ c' ts) | c == c' =
   let evalArg_bwd :: Val 𝔹 × Expl 𝔹 -> Endo (Env 𝔹 × List (Expr 𝔹) × 𝔹)
       evalArg_bwd (v × t) (ρ' × es × α') = (ρ' ∨ ρ'') × (e : es) × (α' ∨ α'')
          where ρ'' × e × α'' = evalBwd v t
       ρ' × es × α' = foldr evalArg_bwd (botOf ρ × Nil × α) (zip vs ts) in
   ρ' × Constr α c es × α'
evalBwd _ (T.Constr _ _ _) =
   error absurd
evalBwd V.Hole t@(T.Matrix tss _ (i' × j') _) =
   evalBwd (V.Matrix false (A.replicate i' (A.replicate j' V.Hole) × (i' × false) × (j' × false))) t
evalBwd (V.Matrix α (vss × (i' × β) × (j' × β'))) (T.Matrix tss (x × y) _ t) =
   let NonEmptyList ijs = nonEmpty $ do
            i <- range 1 i'
            j <- range 1 j'
            singleton (i' × j')
       evalBwd_elem :: (Int × Int) -> Env 𝔹 × Expr 𝔹 × 𝔹 × 𝔹 × 𝔹
       evalBwd_elem (i × j) =
          case evalBwd (vss!(i - 1)!(j - 1)) (tss!(i - 1)!(j - 1)) of
            Extend (Extend ρ (_ ↦ V.Int γ _)) (_ ↦ V.Int γ' _) × e × α' -> ρ × e × α' × γ × γ'
            _ -> error absurd
       ρ × e × α' × γ × γ' = foldl1
         (\(ρ1 × e1 × α1 × γ1 × γ1') (ρ2 × e2 × α2 × γ2 × γ2') ->
            ((ρ1 ∨ ρ2) × (e1 ∨ e2) × (α1 ∨ α2) × (γ1 ∨ γ2) × (γ1' ∨ γ2')))
         (evalBwd_elem <$> ijs)
       ρ' × e' × α'' = evalBwd (V.Constr false cPair (V.Int (γ ∨ β) i' : V.Int (γ' ∨ β') j' : Nil)) t in
   (ρ ∨ ρ') × Matrix α e (x × y) e' × (α ∨ α' ∨ α'')
evalBwd _ (T.Matrix _ _ _ _) =
   error absurd
evalBwd v (T.App (t1 × _ × δ × _) t2 w t3) =
   let ρ1ρ2ρ3 × e × α = evalBwd v t3
       ρ1ρ2 × ρ3 = unmatch ρ1ρ2ρ3 w
       v' × σ = matchBwd ρ3 (ContExpr e) α w
       ρ1 × ρ2 = splitAt (length δ) ρ1ρ2
       ρ' × e2 × α' = evalBwd v' t2
       ρ1' × δ' × α2 = closeDefsBwd ρ2 (ρ1 × δ)
       ρ'' × e1 × α'' = evalBwd (V.Closure (ρ1 ∨ ρ1') δ' σ) t1 in
   (ρ' ∨ ρ'') × App e1 e2 × (α' ∨ α'')
evalBwd v (T.AppPrim (t1 × (PrimOp φ) × vs) (t2 × v2)) =
   let vs' = vs <> singleton v2
       { init: vs'', last: v2' } = fromJust absurd $ unsnoc $
         if φ.arity > L.length vs'
         then case expand v (V.Primitive (PrimOp φ) (const V.Hole <$> vs')) of
            V.Primitive _ vs'' -> vs''
            _ -> error absurd
         else φ.op_bwd v vs'
       ρ × e × α = evalBwd (V.Primitive (PrimOp φ) vs'') t1
       ρ' × e' × α' = evalBwd v2' t2 in
   (ρ ∨ ρ') × App e e' × (α ∨ α')
evalBwd V.Hole t@(T.AppConstr (_ × c × n) _) =
   evalBwd (V.Constr false c (replicate (n + 1) V.Hole)) t
evalBwd (V.Constr β c vs) (T.AppConstr (t1 × _ × n) t2) =
   let { init: vs', last: v2 } = fromJust absurd (unsnoc vs)
       ρ × e × α = evalBwd (V.Constr β c vs') t1
       ρ' × e' × α' = evalBwd v2 t2 in
   (ρ ∨ ρ') × App e e' × (α ∨ α')
evalBwd _ (T.AppConstr _ _) =
   error absurd
evalBwd v (T.Let (T.VarDef w t1) t2) =
   let ρ1ρ2 × e2 × α2 = evalBwd v t2
       ρ1 × ρ2 = unmatch ρ1ρ2 w
       v' × σ = matchBwd ρ2 ContHole α2 w
       ρ1' × e1 × α1 = evalBwd v' t1 in
   (ρ1 ∨ ρ1') × Let (VarDef σ e1) e2 × (α1 ∨ α2)
evalBwd v (T.LetRec δ t) =
   let ρ1ρ2 × e × α = evalBwd v t
       ρ1 × ρ2 = splitAt (length δ) ρ1ρ2
       ρ1' × δ' × α' = closeDefsBwd ρ2 (ρ1 × δ) in
   (ρ1 ∨ ρ1') × LetRec δ' e × (α ∨ α')
