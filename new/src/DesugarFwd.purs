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
import Lattice (𝔹, (∧), bot)
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
      E.Expr (α1 ∧ α2) <$> (E.Let <$> (E.VarDef σ <$> desugarFwd s) <*> (desugarFwd $ Expr α2 (ListComp s_body (NonEmptyList $ q :| qs))))
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList ((Qualifier α1 (Generator p slist)) :| q : qs)))) = do
      e <- desugarFwd $ Expr α2 $ ListComp s_body $ NonEmptyList $ q :| qs
      σ <- desugarFwd $ p × Body e
      -- | What annotation should enil have here?
      let λ = E.Expr (α1 ∧ α2) $ E.Lambda $ totalise σ (enil (α1 ∧ α2))
      eapp (α1 ∧ α2) (evar (α1 ∧ α2) "concat") <$> (eapp (α1 ∧ α2) (eapp (α1 ∧ α2) (evar (α1 ∧ α2) "map") λ) <$> desugarFwd slist)


instance desugarFwdRecDefs :: DesugarFwd (NonEmptyList (Tuple String (Tuple (NonEmptyList Pattern) (Expr Boolean))))
                                         (Bindings Elim Boolean) where
   desugarFwd fπs = fromList <$> toList <$> reverse <$> traverse toRecDef fπss
      where
      fπss = groupBy (eq `on` fst) fπs :: NonEmptyList (NonEmptyList (Clause 𝔹))

      toRecDef :: NonEmptyList (Clause 𝔹) -> MayFail (Binding Elim 𝔹)
      toRecDef fπs' = ((↦) (fst $ head fπs')) <$> desugarFwd (snd <$> fπs')

instance desugarFwdListRest :: DesugarFwd (ListRest Boolean) (E.Expr Boolean) where
   desugarFwd End          = pure (enil bot)
   desugarFwd (Next s l)   = lift2 (econs bot) (desugarFwd s) (desugarFwd l)

-- Cont arguments here act as an accumulator.
instance desugarFwdPatternCont :: DesugarFwd (Tuple Pattern (Cont Boolean)) (Elim Boolean) where
   desugarFwd (PVar x × κ)             = pure $ ElimVar x κ
   desugarFwd (PConstr c πs × κ)       = checkArity c (length πs) *> (ElimConstr <$> singleton c <$> toCont πs)
      where
      toCont :: List Pattern -> MayFail (Cont 𝔹)
      toCont Nil        = pure κ
      toCont (π : πs')  = Arg <$> do
         κ' <- toCont πs'
         desugarFwd $ π × κ'
   desugarFwd (PListEmpty × κ)         = pure $ ElimConstr $ singleton cNil κ
   desugarFwd (PListNonEmpty π o × κ)  = do
      κ' <- Arg <$> desugarFwd (o × κ)
      ElimConstr <$> singleton cCons <$> Arg <$> desugarFwd (π × κ')


instance desugarFwdListPatternRestCont :: DesugarFwd (Tuple ListPatternRest (Cont Boolean)) (Elim Boolean) where
   desugarFwd (PEnd × κ)      = pure $ ElimConstr $ singleton cNil κ
   desugarFwd (PNext π o × κ) = do
      κ' <- Arg <$> desugarFwd (o × κ)
      ElimConstr <$> singleton cCons <$> Arg <$> desugarFwd (π × κ')

instance desugarFwdVarDef :: DesugarFwd (Tuple Pattern (Expr Boolean)) (E.VarDef Boolean) where
   desugarFwd (π × s) = E.VarDef <$> desugarFwd (π × (None :: Cont 𝔹)) <*> desugarFwd s

instance desugarFwdVarDefs :: DesugarFwd (Tuple (NonEmptyList (Tuple Pattern (Expr Boolean))) (Expr Boolean))
                                       (E.Expr Boolean) where
   desugarFwd (NonEmptyList (d :| Nil) × s)     = E.expr <$> (E.Let <$> desugarFwd d <*> desugarFwd s)
   desugarFwd (NonEmptyList (d :| d' : ds) × s) =
      E.expr <$> (E.Let <$> desugarFwd d <*> desugarFwd (NonEmptyList (d' :| ds) × s))

instance desugarFwdBranch :: DesugarFwd (Tuple (NonEmptyList Pattern) (Expr Boolean)) (Elim Boolean) where
   desugarFwd (πs × s) = do
      κ <- Body <$> desugarFwd s
      desugarFwd $ πs × κ

instance desugarFwdBranches :: DesugarFwd (NonEmptyList (NonEmptyList Pattern × Expr Boolean))
                                        (Elim Boolean) where
   desugarFwd bs = do
      NonEmptyList (σ :| σs) <- traverse desugarFwd bs
      foldM maybeJoin σ σs

instance desugarFwdPatternsCont :: DesugarFwd (Tuple (NonEmptyList Pattern) (Cont Boolean)) (Elim Boolean) where
   desugarFwd (NonEmptyList (π :| Nil) × κ)     = desugarFwd $ π × κ
   desugarFwd (NonEmptyList (π :| π' : πs) × κ) = do
      κ' <- Body <$> E.expr <$> E.Lambda <$> desugarFwd (NonEmptyList (π' :| πs) × κ)
      desugarFwd $ π × κ'


class Joinable a where
   maybeJoin :: a -> a -> MayFail a

instance joinableElim :: Joinable (Elim Boolean) where
   maybeJoin (ElimVar x κ) (ElimVar y κ')       = ElimVar <$> x ≞ y <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeJoin κs κs'
   maybeJoin _ _                                = report "Can't join variable and constructor patterns"

instance joinableCont :: Joinable (Cont Boolean) where
   maybeJoin None None                       = pure None
   maybeJoin (Arg σ) (Arg σ')                = Arg <$> maybeJoin σ σ'
   maybeJoin (Body (E.Expr _ (E.Lambda σ)))
             (Body (E.Expr _ (E.Lambda σ'))) = Body<$> (E.expr <$> (E.Lambda <$> maybeJoin σ σ'))
   maybeJoin _ _                             = report "Incompatible continuations"

instance joinableMap :: Joinable (Map Ctr (Cont Boolean)) where
   maybeJoin κs1 κs2 = do
      foldM maybeUpdate κs1 (toUnfoldable κs2 :: List (Ctr × Cont 𝔹))
      where
      maybeUpdate :: Map Ctr (Cont 𝔹) -> Ctr × Cont 𝔹 -> MayFail (Map Ctr (Cont 𝔹))
      maybeUpdate κs (c × κ) =
         case lookup c κs of
            Nothing -> do
               checkDataType "Non-uniform patterns: " c κs
               pure $ insert c κ κs
            Just κ' ->
               update <$> (const <$> pure <$> maybeJoin κ' κ) <@> c <@> κs

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