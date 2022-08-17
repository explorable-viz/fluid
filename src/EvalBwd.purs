module EvalBwd where

import Prelude hiding (absurd)
--import Data.Foldable (length)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:), {-foldr, range, -}reverse{-, singleton, unsnoc, zip-})
--import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty (singleton) as NEL
import Data.Map ({-empty, -}fromFoldable, isEmpty)
--import Data.NonEmpty (foldl1)
import Data.Profunctor.Strong ({-(&&&), -}first)
import Data.Set (singleton)
import Bindings (Bindings, {-Bind, -}Var, (↦){-, (◃), foldBindings-}, key{-, update, val-}, varAnon)
--import DataType (cPair)
--import Expl (Expl(..), VarDef(..)) as T
import Expl ({-Expl, -}Match(..))
import Expr (Cont(..), Elim(..), {-Expr(..), VarDef(..), -}RecDefs, bv)
import Lattice (𝔹, (∨), botOf)
import Util ({-Endo, -}type (×), (×){-, (!)-}, absurd, error{-, definitely'-}, mustLookup{- nonEmpty-}, splitOn, unimplemented)
import Util.SnocList (SnocList(..), (:-), fromList)
--import Util.SnocList (unzip, zip, zipWith) as S
import Val (Env{-, PrimOp(..)-}, SingletonEnv, Val, dom)
import Val (Val(..)) as V

-- second argument contains original environment and recursive definitions
closeDefsBwd :: SingletonEnv 𝔹 -> Env 𝔹 × RecDefs 𝔹 -> Env 𝔹 × RecDefs 𝔹 × 𝔹
closeDefsBwd γ (γ0 × ρ0) =
   case foldlWithIndex joinDefs (Lin × botOf γ0 × botOf ρ0 × false) γ of
   ρ' × γ' × ρ × α -> γ' × (ρ ∨ ρ') × α
   where
   joinDefs :: Var -> RecDefs 𝔹 × Env 𝔹 × RecDefs 𝔹 × 𝔹 -> Val 𝔹 -> RecDefs 𝔹 × Env 𝔹 × RecDefs 𝔹 × 𝔹
   joinDefs f (ρ_acc × γ' × ρ × α) (V.Closure α_f γ_f ρ_f σ_f) =
      (ρ_acc :- f ↦ σ_f) × (γ' ∨ (γ_f <#> NEL.singleton)) × (ρ ∨ ρ_f) × (α ∨ α_f)
   joinDefs _ _ _ = error absurd

matchBwd :: SingletonEnv 𝔹 -> Cont 𝔹 -> 𝔹 -> Match 𝔹 -> Val 𝔹 × Elim 𝔹
matchBwd γ κ _ (MatchVar x) | dom γ == singleton x = mustLookup x γ × ElimVar x κ
matchBwd γ κ _ (MatchVarAnon v) | isEmpty γ        = botOf v × ElimVar varAnon κ
matchBwd ρ κ α (MatchConstr c ws cs)               = V.Constr α c vs × ElimConstr (fromFoldable cκs)
   where vs × κ' = matchArgsBwd ρ κ α (reverse ws # fromList)
         cκs = c × κ' : ((_ × error unimplemented) <$> cs)
matchBwd ρ κ α (MatchRecord xws)                   = V.Record α xvs × ElimRecord xs κ'
   where xvs × κ' = matchRecordBwd ρ κ α xws
         xs = key <$> xws
matchBwd _ _ _ _                                   = error absurd

matchArgsBwd :: SingletonEnv 𝔹 -> Cont 𝔹 -> 𝔹 -> SnocList (Match 𝔹) -> List (Val 𝔹) × Cont 𝔹
matchArgsBwd γ κ _ Lin  | isEmpty γ = Nil × κ
                        | otherwise = error absurd
matchArgsBwd γγ' κ α (ws :- w) =
   let γ × γ'  = splitOn (bv w) γγ'
       v × σ   = matchBwd γ κ α w
       vs × κ' = matchArgsBwd γ' (ContElim σ) α ws in
   (vs <> v : Nil) × κ'

matchRecordBwd :: SingletonEnv 𝔹 -> Cont 𝔹 -> 𝔹 -> Bindings (Match 𝔹) -> Bindings (Val 𝔹) × Cont 𝔹
matchRecordBwd γ κ _ Lin | isEmpty γ   = Lin × κ
                         | otherwise   = error absurd
matchRecordBwd γγ' κ α (xws :- x ↦ w)  =
   let γ × γ'  = splitOn (bv w) γγ'
       v × σ   = matchBwd γ κ α w in
   (first (_ :- x ↦ v)) (matchRecordBwd γ' (ContElim σ) α xws)

{-
evalBwd :: Val 𝔹 -> Expl 𝔹 -> Env 𝔹 × Expr 𝔹 × 𝔹
evalBwd v (T.Var ρ x) = (botOf ρ ◃ x ↦ v) × Var x × false
evalBwd v (T.Op ρ op) = (botOf ρ ◃ op ↦ v) × Op op × false
evalBwd (V.Str α _) (T.Str ρ str) = botOf ρ × Str α str × α
evalBwd (V.Int α _) (T.Int ρ n) = botOf ρ × Int α n × α
evalBwd (V.Float α _) (T.Float ρ n) = botOf ρ × Float α n × α
evalBwd (V.Closure ρ _ α σ) (T.Lambda _ _) = ρ × Lambda σ × α
evalBwd (V.Record α xvs) (T.Record ρ xts) =
   let xs × ts = xts <#> (key &&& val) # S.unzip
       vs = xvs <#> val
       -- Could unify with similar function in constructor case
       evalArg_bwd :: Val 𝔹 × Expl 𝔹 -> Endo (Env 𝔹 × SnocList (Expr 𝔹) × 𝔹)
       evalArg_bwd (v' × t') (ρ' × es × α') = (ρ' ∨ ρ'') × (es :- e) × (α' ∨ α'')
         where ρ'' × e × α'' = evalBwd v' t'
       ρ' × es × α' = foldr evalArg_bwd (botOf ρ × Lin × α) (S.zip vs ts) in
   ρ' × Record α (S.zipWith (↦) xs es) × α'
evalBwd (V.Constr α _ vs) (T.Constr ρ c ts) =
   let evalArg_bwd :: Val 𝔹 × Expl 𝔹 -> Endo (Env 𝔹 × List (Expr 𝔹) × 𝔹)
       evalArg_bwd (v' × t') (ρ' × es × α') = (ρ' ∨ ρ'') × (e : es) × (α' ∨ α'')
          where ρ'' × e × α'' = evalBwd v' t'
       ρ' × es × α' = foldr evalArg_bwd (botOf ρ × Nil × α) (zip vs ts) in
   ρ' × Constr α c es × α'
evalBwd (V.Matrix α (vss × (_ × β) × (_ × β'))) (T.Matrix tss (x × y) (i' × j') t') =
   let NonEmptyList ijs = nonEmpty $ do
            i <- range 1 i'
            j <- range 1 j'
            singleton (i × j)
       evalBwd_elem :: (Int × Int) -> Env 𝔹 × Expr 𝔹 × 𝔹 × 𝔹 × 𝔹
       evalBwd_elem (i × j) =
          case evalBwd (vss!(i - 1)!(j - 1)) (tss!(i - 1)!(j - 1)) of
             (ρ :- _ ↦ V.Int γ _ :- _ ↦ V.Int γ' _) × e × α' -> ρ × e × α' × γ × γ'
             _ -> error absurd
       ρ × e × α' × γ × γ' = foldl1
          (\(ρ1 × e1 × α1 × γ1 × γ1') (ρ2 × e2 × α2 × γ2 × γ2') ->
             ((ρ1 ∨ ρ2) × (e1 ∨ e2) × (α1 ∨ α2) × (γ1 ∨ γ2) × (γ1' ∨ γ2')))
          (evalBwd_elem <$> ijs)
       ρ' × e' × α'' = evalBwd (V.Constr false cPair (V.Int (γ ∨ β) i' : V.Int (γ' ∨ β') j' : Nil)) t' in
    (ρ ∨ ρ') × Matrix α e (x × y) e' × (α ∨ α' ∨ α'')
evalBwd v (T.RecordLookup t xs x) =
   let v' = V.Record false (update (xs <#> (_ ↦ error unimplemented)) (x ↦ v))
       ρ × e × α = evalBwd v' t in
   ρ × RecordLookup e x × α
evalBwd v (T.App (t1 × δ × _) t2 w t3) =
   let ρ1ρ2ρ3 × e × β = evalBwd v t3
       ρ1ρ2 × ρ3 = splitAt (vars w # length) ρ1ρ2ρ3
       v' × σ = matchBwd ρ3 (ContExpr e) β w
       ρ1 × ρ2 = splitAt (length δ) ρ1ρ2
       ρ' × e2 × α = evalBwd v' t2
       ρ1' × δ' × β' = closeDefsBwd ρ2 (ρ1 × δ)
       ρ'' × e1 × α' = evalBwd (V.Closure (ρ1 ∨ ρ1') δ' (β ∨ β') σ) t1 in
   (ρ' ∨ ρ'') × App e1 e2 × (α ∨ α')
evalBwd v@(V.Primitive _ vs'') (T.AppPrim (t1 × PrimOp φ × vs) (t2 × v2)) =
   let vs' = vs <> singleton v2
       { init: vs'', last: v2' } = definitely' $ unsnoc $
         if φ.arity > length vs'
         then vs''
         else φ.op_bwd v vs'
       ρ × e × α = evalBwd (V.Primitive (PrimOp φ) vs'') t1
       ρ' × e' × α' = evalBwd v2' t2 in
   (ρ ∨ ρ') × App e e' × (α ∨ α')
evalBwd (V.Constr β _ vs) (T.AppConstr (t1 × c × _) t2) =
   let { init: vs', last: v2 } = definitely' (unsnoc vs)
       ρ × e × α = evalBwd (V.Constr β c vs') t1
       ρ' × e' × α' = evalBwd v2 t2 in
   (ρ ∨ ρ') × App e e' × (α ∨ α')
evalBwd v (T.Let (T.VarDef w t1) t2) =
   let ρ1ρ2 × e2 × α2 = evalBwd v t2
       ρ1 × ρ2 = splitAt (vars w # length) ρ1ρ2
       v' × σ = matchBwd ρ2 ContNone α2 w
       ρ1' × e1 × α1 = evalBwd v' t1 in
   (ρ1 ∨ ρ1') × Let (VarDef σ e1) e2 × (α1 ∨ α2)
evalBwd v (T.LetRec δ t) =
   let ρ1ρ2 × e × α = evalBwd v t
       ρ1 × ρ2 = splitAt (length δ) ρ1ρ2
       ρ1' × δ' × α' = closeDefsBwd ρ2 (ρ1 × δ) in
   (ρ1 ∨ ρ1') × LetRec δ' e × (α ∨ α')
evalBwd _ _ = error absurd
-}
