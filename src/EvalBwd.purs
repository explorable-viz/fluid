module EvalBwd where

import Prelude hiding (absurd)
import Data.Array (replicate)
import Data.List (List(..), (:), (\\), foldr, range, reverse, singleton, zip)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (fromFoldable)
import Data.NonEmpty (foldl1)
import Bindings (Binding, Bindings(..), (:+:), (↦), (◃), length, find, foldEnv, splitAt, varAnon)
import DataType (cPair, ctrs, dataTypeFor)
import Expl (Expl, Match(..))
import Expl (Expl(..), VarDef(..)) as T
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), RecDefs)
import Lattice (𝔹, botOf, (∨))
import Util (Endo, type (×), (×), (≜), (!), absurd, error, nonEmpty, successful)
import Val (Env, Val, getα, setα)
import Val (Val(..)) as V

unmatch :: Env 𝔹 -> Match 𝔹 -> Env 𝔹 × Env 𝔹
unmatch (ρ :+: x ↦ v) (MatchVar x') = ρ × (Empty :+: (x ≜ x') ↦ v)
unmatch Empty (MatchVar x')         = error absurd
unmatch ρ (MatchVarAnon _)          = ρ × Empty
unmatch ρ (MatchConstr _ ws)        = unmatchArgs ρ (reverse ws)

-- matches are in a reverse order to the original arguments, to correspond with the 'snoc' order of ρ
unmatchArgs :: Env 𝔹 -> List (Match 𝔹) -> Env 𝔹 × Env 𝔹
unmatchArgs ρ Nil       = ρ × Empty
unmatchArgs ρ (w : ws)  = ρ'' × (ρ1 <> ρ2)
   where ρ'  × ρ2 = unmatch ρ w
         ρ'' × ρ1 = unmatchArgs ρ' ws

-- second argument contains original environment and recursive definitions
closeDefs_bwd :: Env 𝔹 -> Env 𝔹 × RecDefs 𝔹 -> Env 𝔹 × RecDefs 𝔹 × 𝔹
closeDefs_bwd ρ (ρ0 × δ0) =
   case foldEnv joinDefs (Empty × botOf ρ0 × botOf δ0 × false) ρ of
   δ' × ρ' × δ × α -> ρ' × (δ ∨ δ') × α
   where
   joinDefs :: Binding Val 𝔹 -> Endo (RecDefs 𝔹 × Env 𝔹 × RecDefs 𝔹 × 𝔹)
   joinDefs (f ↦ V.Closure ρ_f δ_f σ_f) (δ_acc × ρ' × δ × α)
      = (δ_acc :+: f ↦ σ_f) × (ρ' ∨ ρ_f) × (δ ∨ δ_f) × α
   joinDefs (f ↦ V.Hole) (δ_acc × ρ' × δ × α) = (δ_acc :+: f ↦ botOf (successful $ find f δ0)) × ρ' × δ × α
   joinDefs (_ ↦ _) _ = error absurd

match_bwd :: Env 𝔹 -> Cont 𝔹 -> 𝔹 -> Match 𝔹 -> Val 𝔹 × Elim 𝔹
match_bwd (Empty :+: x ↦ v) κ α (MatchVar x')   = v × ElimVar (x ≜ x') κ
match_bwd Empty κ α (MatchVarAnon v)            = botOf v × ElimVar varAnon κ
match_bwd ρ κ α (MatchConstr c ws)              = V.Constr α c vs × ElimConstr (fromFoldable cκs)
   where vs × κ' = matchArgs_bwd ρ κ α (reverse ws)
         cκs = c × κ' : ((_ × ContHole) <$> (ctrs (successful (dataTypeFor c)) \\ singleton c))
match_bwd _ _ _ _                               = error absurd

matchArgs_bwd :: Env 𝔹 -> Cont 𝔹 -> 𝔹 -> List (Match 𝔹) -> List (Val 𝔹) × Cont 𝔹
matchArgs_bwd ρ κ α Nil       = Nil × κ
matchArgs_bwd ρ κ α (w : ws)  =
   let ρ' × ρ1   = unmatch ρ w
       v  × σ    = match_bwd ρ1 κ α w
       vs × κ'   = matchArgs_bwd ρ' (ContElim σ) α ws in
   (vs <> v : Nil) × κ'

eval_bwd :: Val 𝔹 -> Expl 𝔹 -> Env 𝔹 × Expr 𝔹 × 𝔹
eval_bwd v (T.Var ρ x) =
   (botOf ρ ◃ x ↦ v) × Var x × false
eval_bwd v (T.Op ρ op) =
   (botOf ρ ◃ op ↦ v) × Op op × false
eval_bwd V.Hole t@(T.Str _ str) =
   eval_bwd (V.Str false str) t
eval_bwd (V.Str α s) (T.Str ρ s') | s == s' =
   botOf ρ × Str α s × α
eval_bwd _ (T.Str _ _) =
   error absurd
eval_bwd V.Hole t@(T.Int _ n) =
   eval_bwd (V.Int false n) t
eval_bwd (V.Int α n) (T.Int ρ n') | n == n' =
   botOf ρ × Int α n × α
eval_bwd _ (T.Int _ _) =
   error absurd
eval_bwd V.Hole t@(T.Float _ n) =
   eval_bwd (V.Float false n) t
eval_bwd (V.Float α n) (T.Float ρ n') | n == n' =
   botOf ρ × Float α n × α
eval_bwd _ (T.Float _ _) =
   error absurd
eval_bwd V.Hole t@(T.Lambda ρ σ) =
   eval_bwd (V.Closure (botOf ρ) Empty (botOf σ)) t
eval_bwd (V.Closure ρ Empty σ) (T.Lambda _ _) =
   ρ × Lambda σ × false
eval_bwd _ (T.Lambda _ _) =
   error absurd
eval_bwd V.Hole t@(T.Constr _ c ts) =
   eval_bwd (V.Constr false c (ts <#> const V.Hole)) t
eval_bwd (V.Constr α c vs) (T.Constr ρ c' ts) | c == c' =
   let evalArg_bwd :: Val 𝔹 × Expl 𝔹 -> Endo (Env 𝔹 × List (Expr 𝔹) × 𝔹)
       evalArg_bwd (v × t) (ρ' × es × α') = (ρ' ∨ ρ'') × (e : es) × (α' ∨ α'')
          where ρ'' × e × α'' = eval_bwd v t
       ρ' × es × α' = foldr evalArg_bwd (botOf ρ × Nil × α) (zip vs ts) in
   ρ' × Constr α c es × α'
eval_bwd _ (T.Constr _ _ _) =
   error absurd
eval_bwd V.Hole t@(T.Matrix tss _ (i' × j') _) =
   eval_bwd (V.Matrix false (replicate i' (replicate j' V.Hole)) (i' × j')) t
eval_bwd (V.Matrix α vss (i' × j')) (T.Matrix tss (x × y) _ t) =
   let ρ × e × β = eval_bwd (V.Constr false cPair (V.Int α i' : V.Int α j' : Nil)) t
       NonEmptyList ijs = nonEmpty $ do
            i <- range 1 i'
            j <- range 1 j'
            singleton (i' × j')
       eval_bwd_elem (i × j) =
          case eval_bwd (vss!(i - 1)!(j - 1)) (tss!(i - 1)!(j - 1)) of
            Extend (Extend ρ' (_ ↦ V.Int γ _)) (_ ↦ V.Int γ' _) × e' × β' -> ρ' × e' × β' × (γ ∨ γ')
            _ -> error absurd
       ρ' × e' × β' × γ = foldl1
         (\(ρ1 × e1 × β1 × γ1) (ρ2 × e2 × β2 × γ2) -> ((ρ1 ∨ ρ2) × (e1 ∨ e2) × (β1 ∨ β2) × (γ1 ∨ γ2)))
         (eval_bwd_elem <$> ijs) in
   (ρ ∨ ρ') × Matrix (α ∨ γ) e' (x × y) e × (α ∨ β ∨ β')
eval_bwd _ (T.Matrix _ _ _ _) =
   error absurd
eval_bwd v (T.App (t × _ × δ × _) t' w t'') =
   let ρ1ρ2ρ3 × e × α = eval_bwd v t''
       ρ1ρ2 × ρ3 = unmatch ρ1ρ2ρ3 w
       v' × σ = match_bwd ρ3 (ContExpr e) α w
       ρ1 × ρ2 = splitAt (length δ) ρ1ρ2
       ρ' × e' × α' = eval_bwd v' t'
       ρ1' × δ' × α2 = closeDefs_bwd ρ2 (ρ1 × δ)
       ρ'' × e'' × α'' = eval_bwd (V.Closure (ρ1 ∨ ρ1') δ' σ) t in
   (ρ' ∨ ρ'') × App e'' e' × (α' ∨ α'')
eval_bwd v (T.AppPrim (t1 × φ) (t2 × v2)) =
   let β = getα v
       ρ × e × α = eval_bwd (V.Primitive β φ) t1
       ρ' × e' × α' = eval_bwd (setα β v2) t2 in
   (ρ ∨ ρ') × App e e' × (α ∨ α')
eval_bwd v (T.AppConstr (t1 × c × vs) (t2 × v2)) =
   let β = getα v
       ρ × e × α = eval_bwd (V.Constr β c vs) t1
       ρ' × e' × α' = eval_bwd (setα β v2) t2 in
   (ρ ∨ ρ') × App e e' × (α ∨ α')
eval_bwd v (T.BinaryApp (t1 × v1) (op × φ) _ (t2 × v2)) =
   let β = getα v
       ρ × e × α = eval_bwd (setα β v1) t1
       ρ' × e' × α' = eval_bwd (setα β v2) t2 in
   (ρ ∨ ρ' ◃ op ↦ V.Primitive β φ) × BinaryApp e op e' × (α ∨ α')
eval_bwd v (T.Let (T.VarDef w t1) t2) =
   let ρ1ρ2 × e2 × α2 = eval_bwd v t2
       ρ1 × ρ2 = unmatch ρ1ρ2 w
       v' × σ = match_bwd ρ2 ContHole α2 w
       ρ1' × e1 × α1 = eval_bwd v' t1 in
   (ρ1 ∨ ρ1') × Let (VarDef σ e1) e2 × (α1 ∨ α2)
eval_bwd v (T.LetRec δ t) =
   let ρ1ρ2 × e × α = eval_bwd v t
       ρ1 × ρ2 = splitAt (length δ) ρ1ρ2
       ρ1' × δ' × α' = closeDefs_bwd ρ2 (ρ1 × δ) in
   (ρ1 ∨ ρ1') × LetRec δ' e × (α ∨ α')
