module EvalBwd where

import Prelude hiding (absurd)

import Bindings (Var, varAnon)
import Control.Monad.Except (class MonadError)
import Data.Exists (mkExists, runExists)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List (List(..), range, reverse, unsnoc, unzip, zip, (:))
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty (foldl1)
import Data.Profunctor.Strong (second)
import Data.Set (fromFoldable, singleton) as S
import Data.Set (union)
import Data.Tuple (fst, snd, uncurry)
import DataType (cPair)
import Dict (disjointUnion, disjointUnion_inv, empty, get, insert, intersectionWith, isEmpty, keys)
import Dict (fromFoldable, singleton, toUnfoldable) as D
import Effect.Exception (Error)
import Eval (eval)
import Expr (Cont(..), Elim(..), Expr(..), RecDefs, VarDef(..), bv)
import GaloisConnection (GaloisConnection(..))
import Lattice (Raw, bot, botOf, expand, (∨))
import Partial.Unsafe (unsafePartial)
import Trace (AppTrace(..), Trace(..), VarDef(..)) as T
import Trace (AppTrace, ForeignTrace(..), ForeignTrace'(..), Match(..), Trace)
import Util (type (×), Endo, absurd, definitely', error, nonEmpty, successful, (!), (×))
import Util.Pair (zip) as P
import Val (Fun(..), Val(..)) as V
import Val (class Ann, DictRep(..), Env, ForeignOp(..), ForeignOp'(..), MatrixRep(..), Val, append_inv, (<+>))

closeDefsBwd :: forall a. Ann a => Env a -> Env a × RecDefs a × a
closeDefsBwd γ =
   case foldrWithIndex joinDefs (empty × empty × empty × bot) γ of
      ρ' × γ' × ρ × α -> γ' × (ρ ∨ ρ') × α
   where
   joinDefs :: Var -> Val a -> Endo (RecDefs a × Env a × RecDefs a × a)
   joinDefs f _ (ρ_acc × γ' × ρ × α) =
      case get f γ of
         V.Fun α_f (V.Closure γ_f ρ_f σ_f) ->
            (ρ_acc # insert f σ_f) × (γ' ∨ γ_f) × (ρ ∨ ρ_f) × (α ∨ α_f)
         _ -> error absurd

matchBwd :: forall a. Ann a => Env a -> Cont a -> a -> Match -> Val a × Elim a
matchBwd γ κ _ (MatchVar x v)
   | keys γ == S.singleton x = get x γ × ElimVar x κ
   | otherwise = botOf v × ElimVar x κ
matchBwd γ κ _ (MatchVarAnon v)
   | isEmpty γ = botOf v × ElimVar varAnon κ
   | otherwise = error absurd
matchBwd ρ κ α (MatchConstr c ws) = V.Constr α c vs × ElimConstr (D.singleton c κ')
   where
   vs × κ' = matchManyBwd ρ κ α (reverse ws)
matchBwd ρ κ α (MatchRecord xws) = V.Record α (zip xs vs # D.fromFoldable) ×
   ElimRecord (S.fromFoldable $ keys xws) κ'
   where
   xs × ws = xws # D.toUnfoldable # unzip
   vs × κ' = matchManyBwd ρ κ α (ws # reverse)

matchManyBwd :: forall a. Ann a => Env a -> Cont a -> a -> List Match -> List (Val a) × Cont a
matchManyBwd γ κ _ Nil
   | isEmpty γ = Nil × κ
   | otherwise = error absurd
matchManyBwd γγ' κ α (w : ws) =
   (vs <> v : Nil) × κ'
   where
   γ × γ' = disjointUnion_inv (bv w) γγ'
   v × σ = matchBwd γ κ α w
   vs × κ' = matchManyBwd γ' (ContElim σ) α ws

applyBwd :: forall a. Ann a => AppTrace × Val a -> Val a × Val a
applyBwd (T.AppClosure xs w t3 × v) =
   V.Fun (β ∨ β') (V.Closure (γ1 ∨ γ1') δ' σ) × v'
   where
   γ1γ2γ3 × e × β = evalBwd' v t3
   γ1γ2 × γ3 = append_inv (bv w) γ1γ2γ3
   γ1 × γ2 = append_inv xs γ1γ2
   γ1' × δ' × β' = closeDefsBwd γ2
   v' × σ = matchBwd γ3 (ContExpr e) β w
applyBwd (T.AppForeign n (ForeignTrace (id × t)) × v) =
   V.Fun α (V.Foreign φ vs'') × v2'
   where
   φ × α × { init: vs'', last: v2' } = second (second (definitely' <<< unsnoc)) $ runExists applyBwd' t
      where
      applyBwd' :: forall t. ForeignTrace' t -> ForeignOp × a × List (Val _)
      applyBwd' (ForeignTrace' (ForeignOp' φ) t') =
         ForeignOp (id × mkExists (ForeignOp' φ)) ×
            if φ.arity > n then unsafePartial $ let V.Fun α (V.Foreign _ vs'') = v in α × vs''
            else bot × φ.op_bwd (definitely' t' × v)
applyBwd (T.AppConstr c × v) =
   V.Fun β (V.PartialConstr c vs') × v2
   where
   vs × β = case v of
      V.Constr β c' vs | c' == c -> vs × β
      V.Fun β (V.PartialConstr c' vs) | c' == c -> vs × β
      _ -> error absurd
   { init: vs', last: v2 } = definitely' (unsnoc vs)

apply2Bwd :: forall a. Ann a => (AppTrace × AppTrace) × Val a -> Val a × Val a × Val a
apply2Bwd ((t1 × t2) × v) =
   let
      u2 × v2 = applyBwd (t2 × v)
      u1 × v1 = applyBwd (t1 × u2)
   in
      u1 × v1 × v2

evalBwd :: forall a. Ann a => Raw Env -> Raw Expr -> Val a -> Trace -> Env a × Expr a × a
evalBwd γ e v t =
   expand γ' γ × expand e' e × α
   where
   γ' × e' × α = evalBwd' v t

-- Computes a partial slice which evalBwd expands to a full slice.
evalBwd' :: forall a. Ann a => Val a -> Trace -> Env a × Expr a × a
evalBwd' v (T.Var x) = D.singleton x v × Var x × bot
evalBwd' v (T.Op op) = D.singleton op v × Op op × bot
evalBwd' (V.Str α str) T.Const = empty × Str α str × α
evalBwd' (V.Int α n) T.Const = empty × Int α n × α
evalBwd' (V.Float α n) T.Const = empty × Float α n × α
evalBwd' (V.Fun α (V.Closure γ _ σ)) T.Const = γ × Lambda α σ × α
evalBwd' (V.Record α xvs) (T.Record xts) =
   foldr (∨) empty (xγeαs <#> fst)
      × Record α (xγeαs <#> (fst <<< snd))
      × foldr (∨) α (xγeαs <#> (snd <<< snd))
   where
   xvts = intersectionWith (×) xvs xts
   xγeαs = xvts <#> uncurry evalBwd'
evalBwd' (V.Dictionary α (DictRep sαvs)) (T.Dictionary stts sus) =
   foldr (∨) empty ((γeαs <#> fst) <> (γeαs' <#> fst))
      × Dictionary α ((γeαs <#> (fst <<< snd)) `P.zip` (γeαs' <#> (fst <<< snd)))
      × foldr (∨) α ((γeαs <#> (snd <<< snd)) <> (γeαs' <#> (snd <<< snd)))
   where
   sαvs' = expand sαvs (sus <#> (bot × _))
   γeαs = stts <#> \(s × t × _) -> evalBwd' (V.Str (fst (get s sαvs')) s) t
   γeαs' = stts <#> \(s × _ × t) -> evalBwd' (snd (get s sαvs')) t
evalBwd' (V.Constr α _ vs) (T.Constr c ts) =
   γ' × Constr α c es × α'
   where
   evalArg_bwd :: Val a × Trace -> Endo (Env a × List (Expr a) × a)
   evalArg_bwd (v' × t') (γ' × es × α') = (γ' ∨ γ'') × (e : es) × (α' ∨ α'')
      where
      γ'' × e × α'' = evalBwd' v' t'
   γ' × es × α' = foldr evalArg_bwd (empty × Nil × α) (zip vs ts)
evalBwd' (V.Matrix α (MatrixRep (vss × (_ × βi) × (_ × βj)))) (T.Matrix tss (x × y) (i' × j') t') =
   (γ ∨ γ') × Matrix α e (x × y) e' × (α ∨ α' ∨ α'')
   where
   NonEmptyList ijs = nonEmpty do
      i <- range 1 i'
      j <- range 1 j'
      L.singleton (i × j)

   evalBwd_elem :: (Int × Int) -> Env a × Expr a × a × a × a
   evalBwd_elem (i × j) =
      case evalBwd' (vss ! (i - 1) ! (j - 1)) (tss ! (i - 1) ! (j - 1)) of
         γ'' × e × α' ->
            let
               γ × γ' = append_inv (S.singleton x `union` S.singleton y) γ''
               γ0 = (D.singleton x (V.Int bot i') `disjointUnion` D.singleton y (V.Int bot j')) <+> γ'
            in
               unsafePartial $
                  let
                     V.Int β _ × V.Int β' _ = get x γ0 × get y γ0
                  in
                     γ × e × α' × β × β'
   γ × e × α' × β × β' = foldl1
      ( \(γ1 × e1 × α1 × β1 × β1') (γ2 × e2 × α2 × β2 × β2') ->
           ((γ1 ∨ γ2) × (e1 ∨ e2) × (α1 ∨ α2) × (β1 ∨ β2) × (β1' ∨ β2'))
      )
      (evalBwd_elem <$> ijs)
   γ' × e' × α'' =
      evalBwd' (V.Constr bot cPair (V.Int (β ∨ βi) i' : V.Int (β' ∨ βj) j' : Nil)) t'
evalBwd' v (T.Project t x) =
   γ × Project e x × α
   where
   γ × e × α = evalBwd' (V.Record bot (D.singleton x v)) t
evalBwd' v (T.App t1 t2 t3) =
   (γ ∨ γ') × App e e' × (α ∨ α')
   where
   u × v2 = applyBwd (t3 × v)
   γ × e × α = evalBwd' u t1
   γ' × e' × α' = evalBwd' v2 t2
evalBwd' v (T.Let (T.VarDef w t1) t2) =
   (γ1 ∨ γ1') × Let (VarDef σ e1) e2 × α1
   where
   γ1γ2 × e2 × α2 = evalBwd' v t2
   γ1 × γ2 = append_inv (bv w) γ1γ2
   v' × σ = matchBwd γ2 ContNone α2 w
   γ1' × e1 × α1 = evalBwd' v' t1
evalBwd' v (T.LetRec ρ t) =
   (γ1 ∨ γ1') × LetRec (α ∨ α') ρ' e × (α ∨ α')
   where
   γ1γ2 × e × α = evalBwd' v t
   γ1 × γ2 = append_inv (S.fromFoldable $ keys ρ) γ1γ2
   γ1' × ρ' × α' = closeDefsBwd γ2
evalBwd' _ _ = error absurd

type TracedEval a =
   { gc :: GaloisConnection (Env a × Expr a × a) (Val a)
   , v :: Raw Val
   }

traceGC :: forall a m. MonadError Error m => Ann a => Raw Env -> Raw Expr -> m (TracedEval a)
traceGC γ e = do
   t × v <- eval γ e bot
   let
      bwd v' = evalBwd γ e v' t
      fwd (γ' × e' × α) = snd $ successful $ eval γ' e' α
   pure $ { gc: GC { fwd, bwd }, v }
