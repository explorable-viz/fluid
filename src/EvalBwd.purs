module EvalBwd where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Data.Foldable (foldr, length)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:), range, reverse, unsnoc, zip)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (insert, isEmpty)
import Data.Map (singleton) as M
import Data.NonEmpty (foldl1)
import Data.Profunctor.Strong ((&&&), first)
import Data.Set (singleton, union)
import Partial.Unsafe (unsafePartial)
import Bindings (Bindings, Var, (↦), key, val, varAnon)
import Bindings (dom, update) as B
import DataType (cPair)
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), RecDefs, bv)
import Lattice (𝔹, (∨), botOf)
import Trace (Trace(..), VarDef(..)) as T
import Trace (Trace, Match(..))
import Util (Endo, type (×), (×), (!), absurd, error, definitely', disjUnion_inv, mustLookup, nonEmpty)
import Util.SnocList (SnocList(..), (:-), fromList)
import Util.SnocList (singleton, unzip, zip, zipWith) as S
import Val (Env, PrimOp(..), SingletonEnv, Val, asSingleton, concat_inv, dom, update)
import Val (Val(..)) as V

-- second argument contains original environment and recursive definitions
closeDefsBwd :: SingletonEnv 𝔹 -> Env 𝔹 × RecDefs 𝔹 -> Env 𝔹 × RecDefs 𝔹 × 𝔹
closeDefsBwd γ (γ0 × ρ0) =
   case foldlWithIndex joinDefs (Lin × γ0' × ρ0' × false) γ of
   ρ' × γ' × ρ × α -> γ' × (ρ ∨ ρ') × α
   where
   γ0' × ρ0' = botOf γ0 × botOf ρ0
   joinDefs :: Var -> RecDefs 𝔹 × Env 𝔹 × RecDefs 𝔹 × 𝔹 -> Val 𝔹 -> RecDefs 𝔹 × Env 𝔹 × RecDefs 𝔹 × 𝔹
   joinDefs f (ρ_acc × γ' × ρ × α) (V.Closure α_f γ_f ρ_f σ_f) =
      (ρ_acc :- f ↦ σ_f) × (γ' ∨ (γ0' `update` γ_f)) × (ρ ∨ (ρ0' `B.update` ρ_f)) × (α ∨ α_f)
   joinDefs _ _ _ = error absurd

matchBwd :: SingletonEnv 𝔹 -> Cont 𝔹 -> 𝔹 -> Match 𝔹 -> Val 𝔹 × Elim 𝔹
matchBwd γ κ _ (MatchVar x) | dom γ == singleton x = mustLookup x γ × ElimVar x κ
matchBwd γ κ _ (MatchVarAnon v) | isEmpty γ        = botOf v × ElimVar varAnon κ
matchBwd ρ κ α (MatchConstr c ws cκs)              = V.Constr α c vs × ElimConstr (insert c κ' $ (botOf <$> cκs))
   where vs × κ' = matchArgsBwd ρ κ α (reverse ws # fromList)
matchBwd ρ κ α (MatchRecord xws)                   = V.Record α xvs × ElimRecord (key <$> xws) κ'
   where xvs × κ' = matchRecordBwd ρ κ α xws
matchBwd _ _ _ _                                   = error absurd

matchArgsBwd :: SingletonEnv 𝔹 -> Cont 𝔹 -> 𝔹 -> SnocList (Match 𝔹) -> List (Val 𝔹) × Cont 𝔹
matchArgsBwd γ κ _ Lin  | isEmpty γ = Nil × κ
                        | otherwise = error absurd
matchArgsBwd γγ' κ α (ws :- w) =
   let γ × γ'  = disjUnion_inv (bv w) γγ'
       v × σ   = matchBwd γ κ α w
       vs × κ' = matchArgsBwd γ' (ContElim σ) α ws in
   (vs <> v : Nil) × κ'

matchRecordBwd :: SingletonEnv 𝔹 -> Cont 𝔹 -> 𝔹 -> Bindings (Match 𝔹) -> Bindings (Val 𝔹) × Cont 𝔹
matchRecordBwd γ κ _ Lin | isEmpty γ   = Lin × κ
                         | otherwise   = error absurd
matchRecordBwd γγ' κ α (xws :- x ↦ w)  =
   let γ × γ'  = disjUnion_inv (bv w) γγ'
       v × σ   = matchBwd γ κ α w in
   (first (_ :- x ↦ v)) (matchRecordBwd γ' (ContElim σ) α xws)

evalBwd :: Val 𝔹 -> Trace 𝔹 -> Env 𝔹 × Expr 𝔹 × 𝔹
evalBwd v (T.Var γ x) = (botOf γ `update` M.singleton x v) × Var x × false
evalBwd v (T.Op γ op) = (botOf γ `update` M.singleton op v) × Op op × false
evalBwd (V.Str α _) (T.Str γ str) = botOf γ × Str α str × α
evalBwd (V.Int α _) (T.Int γ n) = botOf γ × Int α n × α
evalBwd (V.Float α _) (T.Float γ n) = botOf γ × Float α n × α
evalBwd (V.Closure α γ _ σ) (T.Lambda γ' _) = γ' `update` γ × Lambda σ × α
evalBwd (V.Record α xvs) (T.Record γ xts) =
   let xs × ts = xts <#> (key &&& val) # S.unzip
       vs = xvs <#> val
       -- Could unify with similar function in constructor case
       evalArg_bwd :: Val 𝔹 × Trace 𝔹 -> Endo (Env 𝔹 × SnocList (Expr 𝔹) × 𝔹)
       evalArg_bwd (v' × t') (γ' × es × α') = (γ' ∨ γ'') × (es :- e) × (α' ∨ α'')
         where γ'' × e × α'' = evalBwd v' t'
       γ' × es × α' = foldr evalArg_bwd (botOf γ × Lin × α) (S.zip vs ts) in
   γ' × Record α (S.zipWith (↦) xs es) × α'
evalBwd (V.Constr α _ vs) (T.Constr γ c ts) =
   let evalArg_bwd :: Val 𝔹 × Trace 𝔹 -> Endo (Env 𝔹 × List (Expr 𝔹) × 𝔹)
       evalArg_bwd (v' × t') (γ' × es × α') = (γ' ∨ γ'') × (e : es) × (α' ∨ α'')
          where γ'' × e × α'' = evalBwd v' t'
       γ' × es × α' = foldr evalArg_bwd (botOf γ × Nil × α) (zip vs ts) in
   γ' × Constr α c es × α'
evalBwd (V.Matrix α (vss × (_ × βi) × (_ × βj))) (T.Matrix tss (x × y) (i' × j') t') =
   let NonEmptyList ijs = nonEmpty $ do
            i <- range 1 i'
            j <- range 1 j'
            L.singleton (i × j)
       evalBwd_elem :: (Int × Int) -> Env 𝔹 × Expr 𝔹 × 𝔹 × 𝔹 × 𝔹
       evalBwd_elem (i × j) =
          case evalBwd (vss!(i - 1)!(j - 1)) (tss!(i - 1)!(j - 1)) of
             γ'' × e × α' ->
               let γ × γ' = concat_inv (singleton x `union` singleton y) γ''
               in unsafePartial $ let V.Int β _ × V.Int β' _ = mustLookup x γ' × mustLookup x γ'
               in γ × e × α' × β × β'
       γ × e × α' × β × β' = foldl1
          (\(γ1 × e1 × α1 × β1 × β1') (γ2 × e2 × α2 × β2 × β2') ->
             ((γ1 ∨ γ2) × (e1 ∨ e2) × (α1 ∨ α2) × (β1 ∨ β2) × (β1' ∨ β2')))
          (evalBwd_elem <$> ijs)
       γ' × e' × α'' = evalBwd (V.Constr false cPair (V.Int (β ∨ βi) i' : V.Int (β' ∨ βj) j' : Nil)) t' in
    (γ ∨ γ') × Matrix α e (x × y) e' × (α ∨ α' ∨ α'')
evalBwd v (T.Project t xvs x) =
   let v' = V.Record false $ (xvs <#> botOf) `B.update` (S.singleton $ x ↦ v)
       ρ × e × α = evalBwd v' t in
   ρ × Project e x × α
evalBwd v (T.App (t1 × δ × _) t2 w t3) =
   let γ1γ2γ3 × e × β = evalBwd v t3
       γ1γ2 × γ3 = concat_inv (bv w) γ1γ2γ3
       v' × σ = matchBwd γ3 (ContExpr e) β w
       γ1 × γ2 = concat_inv (B.dom δ) γ1γ2
       γ' × e2 × α = evalBwd v' t2
       γ1' × δ' × β' = closeDefsBwd γ2 (γ1 × δ)
       γ1'' = definitely' $ asSingleton γ1 `lift2 (∨)` asSingleton γ1'
       γ'' × e1 × α' = evalBwd (V.Closure (β ∨ β') γ1'' δ' σ) t1 in
   (γ' ∨ γ'') × App e1 e2 × (α ∨ α')
evalBwd v (T.AppPrim (t1 × PrimOp φ × vs) (t2 × v2)) =
   let vs' = vs <> L.singleton v2
       { init: vs'', last: v2' } = definitely' $ unsnoc $
         if φ.arity > length vs'
         then unsafePartial $ let V.Primitive _ vs'' = v in vs''
         else φ.op_bwd v vs'
       γ × e × α = evalBwd (V.Primitive (PrimOp φ) vs'') t1
       γ' × e' × α' = evalBwd v2' t2 in
   (γ ∨ γ') × App e e' × (α ∨ α')
evalBwd (V.Constr β _ vs) (T.AppConstr (t1 × c × _) t2) =
   let { init: vs', last: v2 } = definitely' (unsnoc vs)
       γ × e × α = evalBwd (V.Constr β c vs') t1
       γ' × e' × α' = evalBwd v2 t2 in
   (γ ∨ γ') × App e e' × (α ∨ α')
evalBwd v (T.Let (T.VarDef w t1) t2) =
   let γ1γ2 × e2 × α2 = evalBwd v t2
       γ1 × γ2 = concat_inv (bv w) γ1γ2
       v' × σ = matchBwd γ2 ContNone α2 w
       γ1' × e1 × α1 = evalBwd v' t1 in
   (γ1 ∨ γ1') × Let (VarDef σ e1) e2 × (α1 ∨ α2)
evalBwd v (T.LetRec ρ t) =
   let γ1γ2 × e × α = evalBwd v t
       γ1 × γ2 = concat_inv (B.dom ρ) γ1γ2
       γ1' × ρ' × α' = closeDefsBwd γ2 (γ1 × ρ) in
   (γ1 ∨ γ1') × LetRec ρ' e × (α ∨ α')
evalBwd _ _ = error absurd
