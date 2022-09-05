module EvalBwd where

import Prelude hiding (absurd)
import Data.Foldable (foldr, length)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List (List(..), (:), range, reverse, unsnoc, unzip, zip)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (empty, fromFoldable, keys, insert, intersectionWith, isEmpty, toUnfoldable)
import Data.Map (singleton) as M
import Data.NonEmpty (foldl1)
import Data.Set (singleton, union)
import Data.Tuple (fst, snd, uncurry)
import Partial.Unsafe (unsafePartial)
import Bindings (Var, varAnon)
import DataType (cPair)
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), bv)
import Lattice (𝔹, (∨), bot, botOf)
import Trace (Trace(..), VarDef(..)) as T
import Trace (Trace, Match(..))
import Util (Endo, type (×), (×), (!), absurd, error, definitely', disjUnion, disjUnion_inv, mustLookup, nonEmpty)
import Val (Env, FunEnv, PrimOp(..), (<+>), Val, (∨∨), append_inv, dom, update)
import Val (Val(..)) as V

closeDefsBwd :: Env 𝔹 -> Env 𝔹 × FunEnv 𝔹 × 𝔹
closeDefsBwd γ =
   case foldrWithIndex joinDefs (empty × empty × empty × false) γ of
   ρ' × γ' × ρ × α -> γ' × (ρ ∨ ρ') × α
   where
   joinDefs :: Var -> Val 𝔹 -> Endo (FunEnv 𝔹 × Env 𝔹 × FunEnv 𝔹 × 𝔹)
   joinDefs f _ (ρ_acc × γ' × ρ × α) =
      case mustLookup f γ of
         V.Closure α_f γ_f ρ_f σ_f ->
            (ρ_acc # insert f σ_f) × (γ' ∨∨ γ_f) × (ρ ∨∨ ρ_f) × (α ∨ α_f)
         _ -> error absurd

matchBwd :: Env 𝔹 -> Cont 𝔹 -> 𝔹 -> Match 𝔹 -> Val 𝔹 × Elim 𝔹
matchBwd γ κ _ (MatchVar x v)
   | dom γ == singleton x           = mustLookup x γ × ElimVar x κ
   | otherwise                      = botOf v × ElimVar x κ
matchBwd γ κ _ (MatchVarAnon v)
   | isEmpty γ                      = botOf v × ElimVar varAnon κ
   | otherwise                      = error absurd
matchBwd ρ κ α (MatchConstr c ws)   = V.Constr α c vs × ElimConstr (M.singleton c κ')
   where vs × κ' = matchManyBwd ρ κ α (reverse ws)
matchBwd ρ κ α (MatchRecord xws)    = V.Record α (zip xs vs # fromFoldable) × ElimRecord (keys xws) κ'
   where xs × ws = xws # toUnfoldable # unzip
         vs × κ' = matchManyBwd ρ κ α (ws # reverse)

matchManyBwd :: Env 𝔹 -> Cont 𝔹 -> 𝔹 -> List (Match 𝔹) -> List (Val 𝔹) × Cont 𝔹
matchManyBwd γ κ _ Nil  | isEmpty γ = Nil × κ
                        | otherwise = error absurd
matchManyBwd γγ' κ α (w : ws) =
   let γ × γ'  = disjUnion_inv (bv w) γγ'
       v × σ   = matchBwd γ κ α w
       vs × κ' = matchManyBwd γ' (ContElim σ) α ws in
   (vs <> v : Nil) × κ'

evalBwd :: Val 𝔹 -> Trace 𝔹 -> Env 𝔹 × Expr 𝔹 × 𝔹
evalBwd v (T.Var x) = M.singleton x v × Var x × false
evalBwd v (T.Op op) = M.singleton op v × Op op × false
evalBwd (V.Str α _) (T.Str str) = empty × Str α str × α
evalBwd (V.Int α _) (T.Int n) = empty × Int α n × α
evalBwd (V.Float α _) (T.Float n) = empty × Float α n × α
evalBwd (V.Closure α γ _ σ) (T.Lambda _) = γ × Lambda σ × α
evalBwd (V.Record α xvs) (T.Record γ xts) =
   let xvts = intersectionWith (×) xvs xts
       xγeαs = xvts <#> uncurry evalBwd
       γ' = foldr (∨) (botOf γ) (xγeαs <#> (fst >>> fst)) in
   γ' × Record α (xγeαs <#> (fst >>> snd)) × (foldr (∨) α (xγeαs <#> snd))
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
               let γ × γ' = append_inv (singleton x `union` singleton y) γ''
                   γ0 = (M.singleton x (V.Int bot i') `disjUnion` M.singleton y (V.Int bot j')) <+> γ'
               in unsafePartial $ let V.Int β _ × V.Int β' _ = mustLookup x γ0 × mustLookup x γ0
               in γ × e × α' × β × β'
       γ × e × α' × β × β' = foldl1
          (\(γ1 × e1 × α1 × β1 × β1') (γ2 × e2 × α2 × β2 × β2') ->
             ((γ1 ∨ γ2) × (e1 ∨ e2) × (α1 ∨ α2) × (β1 ∨ β2) × (β1' ∨ β2')))
          (evalBwd_elem <$> ijs)
       γ' × e' × α'' = evalBwd (V.Constr false cPair (V.Int (β ∨ βi) i' : V.Int (β' ∨ βj) j' : Nil)) t' in
    (γ ∨ γ') × Matrix α e (x × y) e' × (α ∨ α' ∨ α'')
evalBwd v (T.Project t x) =
   let v' = V.Record false (M.singleton x v)
       ρ × e × α = evalBwd v' t in
   ρ × Project e x × α
evalBwd v (T.App (t1 × xs × _) t2 w t3) =
   let γ1γ2γ3 × e × β = evalBwd v t3
       γ1γ2 × γ3 = append_inv (bv w) γ1γ2γ3
       v' × σ = matchBwd γ3 (ContExpr e) β w
       γ1 × γ2 = append_inv xs γ1γ2
       γ' × e2 × α = evalBwd v' t2
       γ1' × δ' × β' = closeDefsBwd γ2
       γ'' × e1 × α' = evalBwd (V.Closure (β ∨ β') (γ1 ∨ γ1') δ' σ) t1 in
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
       γ1 × γ2 = append_inv (bv w) γ1γ2
       v' × σ = matchBwd γ2 ContNone α2 w
       γ1' × e1 × α1 = evalBwd v' t1 in
   (γ1 ∨ γ1') × Let (VarDef σ e1) e2 × (α1 ∨ α2)
evalBwd v (T.LetRec ρ t) =
   let γ1γ2 × e × α = evalBwd v t
       γ1 × γ2 = append_inv (keys ρ) γ1γ2
       γ1' × ρ' × α' = closeDefsBwd γ2 in
   (γ1 ∨ γ1') × LetRec ((botOf ρ # toUnfoldable) `update` ρ' # fromFoldable) e × (α ∨ α')
evalBwd _ _ = error absurd
