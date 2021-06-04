module EvalBwd where

import Prelude hiding (absurd)
import Data.Foldable (length)
import Data.List (List(..), (:), foldr, range, reverse, singleton, unsnoc, zip)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (fromFoldable)
import Data.NonEmpty (foldl1)
import Data.Profunctor.Strong (first)
import Bindings (Binding, Bindings(..), (:+:), (↦), (◃), foldBindings, toSnocList, varAnon)
import Bindings2 (Bindings2, asBindings, asBindings2)
import Bindings2 ((↦)) as B
import DataType (cPair)
import Expl (Expl(..), VarDef(..)) as T
import Expl (Expl, Match(..), vars)
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), RecDefs)
import Lattice (𝔹, (∨), botOf, expand)
import Util (Endo, type (×), (×), (≜), (!), absurd, error, fromJust, nonEmpty, replicate)
import Util.SnocList (SnocList(..), (:-), fromList, splitAt)
import Val (Env, Env2, PrimOp(..), Val, holeMatrix)
import Val (Val(..)) as V

-- second argument contains original environment and recursive definitions
closeDefsBwd :: Env 𝔹 -> Env 𝔹 × RecDefs 𝔹 -> Env 𝔹 × RecDefs 𝔹
closeDefsBwd ρ (ρ0 × δ0) =
   case foldBindings joinDefs (Empty × botOf ρ0 × botOf δ0) ρ of
   δ' × ρ' × δ -> ρ' × (δ ∨ δ')
   where
   joinDefs :: Binding Val 𝔹 -> Endo (RecDefs 𝔹 × Env 𝔹 × RecDefs 𝔹)
   joinDefs (f ↦ v) (δ_acc × ρ' × δ) =
      case expand v (V.Closure (asBindings2 (botOf ρ')) (asBindings2 (botOf δ)) (ElimHole false)) of
         V.Closure ρ_f δ_f σ_f -> (δ_acc :+: f ↦ σ_f) × (ρ' ∨ asBindings ρ_f) × (δ ∨ asBindings δ_f)
         _ -> error absurd

matchBwd :: Env2 𝔹 -> Cont 𝔹 -> 𝔹 -> Match 𝔹 -> Val 𝔹 × Elim 𝔹
matchBwd (Lin :- x B.↦ v) κ α (MatchVar x') = v × ElimVar (x ≜ x') κ
matchBwd Lin κ α (MatchVarAnon v)          = botOf v × ElimVar varAnon κ
matchBwd ρ κ α (MatchConstr c ws cs)         = V.Constr α c vs × ElimConstr (fromFoldable cκs)
   where vs × κ' = matchArgsBwd ρ κ α (reverse ws # fromList)
         cκs = c × κ' : ((_ × ContHole false) <$> cs)
matchBwd ρ κ α (MatchRecord xws)             = V.Record α xvs × ElimRecord xs κ'
   where xvs × κ' = matchRecordBwd ρ κ α xws
         xs = (\(x ↦ _) -> x) <$> toSnocList (asBindings xws)
matchBwd _ _ _ _                             = error absurd

matchArgsBwd :: Env2 𝔹 -> Cont 𝔹 -> 𝔹 -> SnocList (Match 𝔹) -> List (Val 𝔹) × Cont 𝔹
matchArgsBwd Lin κ α Lin       = Nil × κ
matchArgsBwd (_ :- _) κ α Lin   = error absurd
matchArgsBwd ρρ' κ α (ws :- w) =
   let ρ × ρ'  = splitAt (vars w # length) ρρ'
       v × σ   = matchBwd ρ' κ α w
       vs × κ' = matchArgsBwd ρ (ContElim σ) α ws in
   (vs <> v : Nil) × κ'

matchRecordBwd :: Env2 𝔹 -> Cont 𝔹 -> 𝔹 -> Bindings2 (Match 𝔹) -> Bindings2 (Val 𝔹) × Cont 𝔹
matchRecordBwd Lin κ α Lin         = Lin × κ
matchRecordBwd (_ :- _) κ α Lin    = error absurd
matchRecordBwd ρρ' κ α (xws :- x B.↦ w) =
   let ρ × ρ'  = splitAt (vars w # length) ρρ'
       v × σ   = matchBwd ρ' κ α w in
   (first (_ :- x B.↦ v)) (matchRecordBwd ρ (ContElim σ) α xws)

evalBwd :: Val 𝔹 -> Expl 𝔹 -> Env 𝔹 × Expr 𝔹 × 𝔹
evalBwd v (T.Var ρ x) = (asBindings (botOf ρ) ◃ x ↦ v) × Var x × false
evalBwd v (T.Op ρ op) = (asBindings (botOf ρ) ◃ op ↦ v) × Op op × false
evalBwd v t@(T.Str ρ str) =
   case expand v (V.Str false str) of
      V.Str α _ -> asBindings (botOf ρ) × Str α str × α
      _ -> error absurd
evalBwd v t@(T.Int ρ n) =
   case expand v (V.Int false n) of
      V.Int α _ -> asBindings (botOf ρ) × Int α n × α
      _ -> error absurd
evalBwd v t@(T.Float ρ n) =
   case expand v (V.Float false n) of
      V.Float α _ -> asBindings (botOf ρ) × Float α n × α
      _ -> error absurd
evalBwd v t@(T.Lambda ρ σ) =
   case expand v (V.Closure (botOf ρ) Lin (botOf σ)) of
      V.Closure ρ' _ σ' -> asBindings ρ' × Lambda σ' × false
      _ -> error absurd
evalBwd v t@(T.Record ρ xts) =
   error "todo"
evalBwd v t@(T.Constr ρ c ts) =
   case expand v (V.Constr false c (ts <#> const (V.Hole false))) of
      V.Constr α _ vs ->
         let evalArg_bwd :: Val 𝔹 × Expl 𝔹 -> Endo (Env 𝔹 × List (Expr 𝔹) × 𝔹)
             evalArg_bwd (v' × t') (ρ' × es × α') = (ρ' ∨ ρ'') × (e : es) × (α' ∨ α'')
               where ρ'' × e × α'' = evalBwd v' t'
             ρ' × es × α' = foldr evalArg_bwd (asBindings (botOf ρ) × Nil × α) (zip vs ts) in
         ρ' × Constr α c es × α'
      _ -> error absurd
evalBwd v t@(T.Matrix tss (x × y) (i' × j') t') =
   case expand v (V.Matrix false (holeMatrix i' j')) of
      V.Matrix α (vss × (_ × β) × (_ × β')) ->
         let NonEmptyList ijs = nonEmpty $ do
                  i <- range 1 i'
                  j <- range 1 j'
                  singleton (i × j)
             evalBwd_elem :: (Int × Int) -> Env 𝔹 × Expr 𝔹 × 𝔹 × 𝔹 × 𝔹
             evalBwd_elem (i × j) =
                case evalBwd (vss!(i - 1)!(j - 1)) (tss!(i - 1)!(j - 1)) of
                   Extend (Extend ρ (_ ↦ v1)) (_ ↦ v2) × e × α' ->
                      case expand v1 (V.Int false i) × expand v2 (V.Int false j) of
                         V.Int γ _ × V.Int γ' _ -> ρ × e × α' × γ × γ'
                         _ -> error absurd
                   _ -> error absurd
             ρ × e × α' × γ × γ' = foldl1
                (\(ρ1 × e1 × α1 × γ1 × γ1') (ρ2 × e2 × α2 × γ2 × γ2') ->
                   ((ρ1 ∨ ρ2) × (e1 ∨ e2) × (α1 ∨ α2) × (γ1 ∨ γ2) × (γ1' ∨ γ2')))
                (evalBwd_elem <$> ijs)
             ρ' × e' × α'' = evalBwd (V.Constr false cPair (V.Int (γ ∨ β) i' : V.Int (γ' ∨ β') j' : Nil)) t' in
          (ρ ∨ ρ') × Matrix α e (x × y) e' × (α ∨ α' ∨ α'')
      _ -> error absurd
evalBwd v (T.App (t1 × _ × δ × _) t2 w t3) =
   let ρ1ρ2ρ3 × e × α = evalBwd v t3
       ρ1ρ2 × ρ3 = splitAt (vars w # length) (asBindings2 ρ1ρ2ρ3)
       v' × σ = matchBwd ρ3 (ContExpr e) α w
       ρ1 × ρ2 = splitAt (length δ) ρ1ρ2
       ρ' × e2 × α' = evalBwd v' t2
       ρ1' × δ' = closeDefsBwd (asBindings ρ2) (asBindings ρ1 × asBindings δ)
       ρ'' × e1 × α'' = evalBwd (V.Closure (ρ1 ∨ asBindings2 ρ1') (asBindings2 δ') σ) t1 in
   (ρ' ∨ ρ'') × App e1 e2 × (α' ∨ α'')
evalBwd v (T.AppPrim (t1 × PrimOp φ × vs) (t2 × v2)) =
   let vs' = vs <> singleton v2
       { init: vs'', last: v2' } = fromJust absurd $ unsnoc $
         if φ.arity > length vs'
         then case expand v (V.Primitive (PrimOp φ) (const (V.Hole false) <$> vs')) of
            V.Primitive _ vs'' -> vs''
            _ -> error absurd
         else φ.op_bwd (v × φ.op vs') vs'
       ρ × e × α = evalBwd (V.Primitive (PrimOp φ) vs'') t1
       ρ' × e' × α' = evalBwd v2' t2 in
   (ρ ∨ ρ') × App e e' × (α ∨ α')
evalBwd v t@(T.AppConstr (t1 × c × n) t2) =
   case expand v (V.Constr false c (replicate (n + 1) (V.Hole false))) of
      V.Constr β _ vs ->
         let { init: vs', last: v2 } = fromJust absurd (unsnoc vs)
             ρ × e × α = evalBwd (V.Constr β c vs') t1
             ρ' × e' × α' = evalBwd v2 t2 in
         (ρ ∨ ρ') × App e e' × (α ∨ α')
      _ -> error absurd
evalBwd v (T.Let (T.VarDef w t1) t2) =
   let ρ1ρ2 × e2 × α2 = evalBwd v t2
       ρ1 × ρ2 = splitAt (vars w # length) (asBindings2 ρ1ρ2)
       v' × σ = matchBwd ρ2 (ContHole false) α2 w
       ρ1' × e1 × α1 = evalBwd v' t1 in
   (asBindings ρ1 ∨ ρ1') × Let (VarDef σ e1) e2 × (α1 ∨ α2)
evalBwd v (T.LetRec δ t) =
   let ρ1ρ2 × e × α = evalBwd v t
       ρ1 × ρ2 = splitAt (length δ) (asBindings2 ρ1ρ2)
       ρ1' × δ' = closeDefsBwd (asBindings ρ2) (asBindings ρ1 × asBindings δ) in
   (asBindings ρ1 ∨ ρ1') × LetRec (asBindings2 δ') e × α
