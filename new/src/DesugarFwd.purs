module DesugarFwd where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Function (on)
import Data.List (List(..), (:), (\\), length)
import Data.List (head) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, reverse, toList)
import Data.Map (Map, fromFoldable, insert, lookup, singleton, toUnfoldable, update)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)
import Bindings (Binding, Bindings, (↦), fromList)
import DataType (Ctr, DataType'(..), checkArity, checkDataType, ctrToDataType, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), Var)
import Expr (Expr(..), Module(..), RawExpr(..), VarDef(..), expr) as E
import SExprX (
   Clause, Expr(..), ListPatternRest(..), ListRest(..), Module(..), Pattern(..), RawQualifier(..), Qualifier(..), RawExpr(..), expr
)
import Lattice (𝔹, (∧))
import Util (MayFail, type (×), (×), (≞), absurd, fromJust, mustLookup, report)

eapp :: 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹
eapp α f = E.Expr α <<< E.App f

enil :: 𝔹 -> E.Expr 𝔹
enil α = E.Expr α $ E.Constr cNil Nil

econs :: 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹
econs α e e' = E.Expr α $ E.Constr cCons (e : e' : Nil)

evar :: 𝔹 -> Var -> E.Expr 𝔹
evar α = E.Expr α <<< E.Var

class DesugarFwd a b | a -> b where
   desugarFwd :: a -> MayFail b

instance desugarFwdExpr :: DesugarFwd (Expr Boolean) (E.Expr Boolean) where
   desugarFwd (Expr α (Int n))               = pure $ E.Expr α (E.Int n)
   desugarFwd (Expr α (Float n))             = pure $ E.Expr α (E.Float n)
   desugarFwd (Expr α (Var x))               = pure $ E.Expr α (E.Var x)
   desugarFwd (Expr α (Op op))               = pure $ E.Expr α (E.Op op)
   desugarFwd (Expr α (Str s))               = pure $ E.Expr α (E.Str s)
   desugarFwd (Expr α (Constr ctr args))     = E.Expr α <$> (E.Constr ctr <$> traverse desugarFwd args)
   desugarFwd (Expr α (Lambda bs))           = E.Expr α <$> (E.Lambda <$> desugarFwd bs)
   desugarFwd (Expr α (App s1 s2))           = E.Expr α <$> (E.App <$> desugarFwd s1 <*> desugarFwd s2)
   desugarFwd (Expr α (BinaryApp s1 op s2))  = E.Expr α <$> (E.BinaryApp <$> desugarFwd s1 <@> op <*> desugarFwd s2)
   desugarFwd (Expr α (MatchAs s bs))        = E.Expr α <$> (E.App <$> (E.Expr α <$> E.Lambda <$> desugarFwd bs) <*> desugarFwd s)
   -- | The α here is not propagated due to how desugarVarDefs is defined
   desugarFwd (Expr α (Let ds s))            = desugarFwd $ ds × s
   desugarFwd (Expr α (LetRec fπs s))        = E.Expr α <$> (E.LetRec <$> desugarFwd fπs <*> desugarFwd s)
   desugarFwd (Expr α (IfElse s1 s2 s3)) = do
      e2 <- desugarFwd s2
      e3 <- desugarFwd s3
      let σ = ElimConstr (fromFoldable [cTrue × Body e2, cFalse × Body e3])
      E.Expr α <$> (E.App (E.Expr α $ E.Lambda σ) <$> desugarFwd s1)
   desugarFwd (Expr α (ListEmpty))           = pure $ enil α
   desugarFwd (Expr α (ListNonEmpty s l))    = lift2 (econs α) (desugarFwd s) (desugarFwd l)
   desugarFwd (Expr α (ListRange s1 s2)) =
      eapp α <$> ((eapp α (evar α "range")) <$> desugarFwd s1) <*> desugarFwd s2
   desugarFwd (Expr α1 (ListComp s_body (NonEmptyList (Qualifier _ (Guard (Expr α2 (Constr c Nil))) :| Nil)))) | c == cTrue = do
      e <- desugarFwd s_body
      pure $ econs (α1 ∧ α2) e (enil (α1 ∧ α2))
   desugarFwd (Expr α (ListComp s_body (NonEmptyList (q :| Nil)))) =
      desugarFwd $ Expr α $ ListComp s_body $ NonEmptyList $ q :| (Qualifier α (Guard (Expr α $ Constr cTrue Nil))) : Nil
   -- | The definition of list-comp-guard is different than that of the paper; need to check annotations
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList ((Qualifier α1 (Guard s)) :| q : qs)))) = do
      e <- desugarFwd $ Expr α2 $ ListComp s_body $ NonEmptyList $ q :| qs
      let σ = ElimConstr (fromFoldable [cTrue × Body e, cFalse × Body (enil (α1 ∧ α2))])
      E.Expr (α1 ∧ α2) <$> (E.App (E.Expr (α1 ∧ α2) $ E.Lambda σ) <$> desugarFwd s)
   -- | The definition of list-comp-decl is different than that of the paper; need to check annotations
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList ((Qualifier α1 (Declaration (p × s))) :| q : qs)))) = do
      σ <- desugarFwd $ p × (None :: Cont 𝔹)
      e <- desugarFwd $ Expr α2 (ListComp s_body (NonEmptyList $ q :| qs))
      E.Expr (α1 ∧ α2) <$> (E.Let <$> (E.VarDef σ <$> desugarFwd s) <*> e)
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList ((Qualifier α1 (Generator p slist)) :| q : qs)))) = do
      e <- desugarFwd $ Expr α2 $ ListComp s_body $ NonEmptyList $ q :| qs
      σ <- desugarFwd $ p × Body e
      -- | What annotation should enil have here?
      let λ = E.Expr (α1 ∧ α2) $ E.Lambda $ totalise σ (enil (α1 ∧ α2))
      eapp (α1 ∧ α2) (evar (α1 ∧ α2) "concat") <$> (eapp (α1 ∧ α2) (eapp (α1 ∧ α2) (evar (α1 ∧ α2) "map") λ) <$> desugarFwd slist)

totalise :: Elim 𝔹 -> E.Expr 𝔹 -> Elim 𝔹
totalise (ElimConstr m) e =
   let c × κ            = fromJust absurd $ L.head $ toUnfoldable m
       bs               = toUnfoldable m
       DataType _ sigs  = mustLookup c ctrToDataType
       bs'              = (_ × Body e) <$> ((fst <$> toUnfoldable sigs) \\ (fst <$> bs))
       bs''             = bs <#> \(c × κ) -> case mustLookup c m of
                           Arg σ   -> c × Arg (totalise σ e)
                           Body e' -> c × Body e'
                           None    -> c × Body e
     in   ElimConstr $ fromFoldable $ bs'' <> bs'
totalise (ElimVar e κ) e' = case κ of
   Arg σ  -> ElimVar e $ Arg $ totalise σ e'
   Body _ -> ElimVar e κ
   None   -> ElimVar e $ Body e'