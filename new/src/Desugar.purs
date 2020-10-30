module Desugar where

import Prelude hiding (absurd)
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
import SExpr (Clause, Expr(..), Module(..), Pattern(..), Qualifier(..), RawExpr(..), expr)
import Lattice (𝔹)
import Util (MayFail, type (×), (×), (≞), absurd, error, fromJust, mustLookup, report)

eapp :: E.Expr 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹
eapp f = E.expr <<< E.App f

enil :: E.Expr 𝔹
enil = E.expr $ E.Constr cNil Nil

evar :: Var -> E.Expr 𝔹
evar = E.expr <<< E.Var

class Desugarable a b where
   desugar :: a -> MayFail b

instance desugarVarDef :: Desugarable (Tuple Pattern (Expr Boolean)) (E.VarDef Boolean) where
   desugar (π × s) = E.VarDef <$> desugar (π × (None :: Cont 𝔹)) <*> desugar s

instance desugarRecDefs :: Desugarable (NonEmptyList (Tuple String (Tuple (NonEmptyList Pattern) (Expr Boolean))))
                                       (Bindings Elim Boolean) where
   desugar fπs = fromList <$> toList <$> reverse <$> traverse toRecDef fπss
      where
      fπss = groupBy (eq `on` fst) fπs :: NonEmptyList (NonEmptyList (Clause 𝔹))

      toRecDef :: NonEmptyList (Clause 𝔹) -> MayFail (Binding Elim 𝔹)
      toRecDef fπs' = ((↦) (fst $ head fπs')) <$> desugar (snd <$> fπs')

instance desugarVarDefs :: Desugarable (Tuple (NonEmptyList (Tuple Pattern (Expr Boolean))) (Expr Boolean))
                                       (E.Expr Boolean) where
   desugar (NonEmptyList (d :| Nil) × s)     = E.expr <$> (E.Let <$> desugar d <*> desugar s)
   desugar (NonEmptyList (d :| d' : ds) × s) =
      E.expr <$> (E.Let <$> desugar d <*> desugar (NonEmptyList (d' :| ds) × s))

instance desugarExpr :: Desugarable (Expr Boolean) (E.Expr Boolean) where
   desugar (Expr _ (Int n))               = pure $ E.expr (E.Int n)
   desugar (Expr _ (Float n))             = pure $ E.expr (E.Float n)
   desugar (Expr _ (Var x))               = pure $ E.expr (E.Var x)
   desugar (Expr _ (Op op))               = pure $ E.expr (E.Op op)
   desugar (Expr _ (Str s))               = pure $ E.expr (E.Str s)
   desugar (Expr _ (Constr ctr args))     = E.expr <$> (E.Constr ctr <$> traverse desugar args)
   desugar (Expr _ (Lambda bs))           = E.expr <$> (E.Lambda <$> desugar bs)
   desugar (Expr _ (App s1 s2))           = E.expr <$> (E.App <$> desugar s1 <*> desugar s2)
   desugar (Expr _ (BinaryApp s1 op s2))  = E.expr <$> (E.BinaryApp <$> desugar s1 <@> op <*> desugar s2)
   desugar (Expr _ (MatchAs s bs))        = E.expr <$> (E.App <$> (E.expr <$> E.Lambda <$> desugar bs) <*> desugar s)
   desugar (Expr _ (Let ds s))            = desugar $ ds × s
   desugar (Expr _ (LetRec fπs s))        = E.expr <$> (E.LetRec <$> desugar fπs <*> desugar s)
   desugar (Expr _ (IfElse s1 s2 s3)) = do
      e2 <- desugar s2
      e3 <- desugar s3
      let σ = ElimConstr (fromFoldable [cTrue × Body e2, cFalse × Body e3])
      E.expr <$> (E.App (E.expr $ E.Lambda σ) <$> desugar s1)
   desugar (Expr _ (ListRange s1 s2)) =
      eapp <$> (eapp (evar "range") <$> desugar s1) <*> desugar s2
   desugar (Expr _ (ListComp s_body (Guard (Expr _ (Constr cTrue Nil)) : Nil))) = do
      e <- desugar s_body
      pure $ E.expr $ E.Constr cCons (e : enil : Nil)
   desugar (Expr _ (ListComp s_body (q:Nil))) =
      desugar $ expr $ ListComp s_body $ q : Guard (expr $ Constr cTrue Nil) : Nil
   desugar (Expr _ (ListComp s_body (Guard s : qs))) = do
      e <- desugar $ expr $ ListComp s_body qs
      let σ = ElimConstr (fromFoldable [cTrue × Body e, cFalse × Body enil])
      E.expr <$> (E.App (E.expr $ E.Lambda σ) <$> desugar s)
   desugar (Expr _ (ListComp s_body (Generator p slist : qs))) = do
      e <- desugar $ expr $ ListComp s_body qs
      σ <- desugar $ p × (Body e :: Cont 𝔹)
      let λ = E.expr $ E.Lambda $ totalise σ enil
      eapp (evar "concat") <$> (eapp (eapp (evar "map") λ) <$> desugar slist)
   desugar (Expr _ (ListComp s_body (Declaration p s : qs))) = do
      σ <- desugar $ p × (None :: Cont 𝔹)
      E.expr <$> (E.Let <$> (E.VarDef σ <$> desugar s) <*> desugar (expr $ ListComp s_body qs))
   desugar (Expr _ (ListComp _ Nil)) = error absurd

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

instance desugarEither :: (Desugarable a b, Desugarable c d) => Desugarable (Either a c) (Either b d) where
   desugar (Left x) = Left <$> desugar x
   desugar (Right x) = Right <$> desugar x

instance desugarModule :: Desugarable (Module Boolean) (E.Module Boolean) where
   desugar (Module ds) = E.Module <$> traverse desugar (join $ ds <#> burble)
      where
      burble (Left ds') = toList ds' <#> Left
      burble (Right δ)  = pure $ Right δ

-- The Cont arguments here act as an accumulator.
instance desugarPattern :: Desugarable (Tuple Pattern (Cont Boolean)) (Elim Boolean) where
   desugar (PVar x × κ)       = pure $ ElimVar x κ
   desugar (PConstr c πs × κ) = checkArity c (length πs) *> (ElimConstr <$> singleton c <$> toCont πs)
      where
      toCont :: List Pattern -> MayFail (Cont 𝔹)
      toCont Nil        = pure κ
      toCont (π : πs')  = Arg <$> do
         κ' <- toCont πs'
         desugar $ π × κ'

instance desugarPatterns :: Desugarable (Tuple (NonEmptyList Pattern) (Cont Boolean)) (Elim Boolean) where
   desugar (NonEmptyList (π :| Nil) × κ)     = desugar $ π × κ
   desugar (NonEmptyList (π :| π' : πs) × κ) = do
      κ' <- Body <$> E.expr <$> E.Lambda <$> desugar (NonEmptyList (π' :| πs) × κ) :: MayFail (Cont 𝔹)
      desugar $ π × κ'

instance desugarBranch :: Desugarable (Tuple (NonEmptyList Pattern) (Expr Boolean)) (Elim Boolean) where
   desugar (πs × s) = do
      κ <- Body <$> desugar s :: MayFail (Cont 𝔹)
      desugar $ πs × κ

instance desugarBranches :: Desugarable (NonEmptyList (NonEmptyList Pattern × Expr Boolean))
                                        (Elim Boolean) where
   desugar bs = do
      NonEmptyList (σ :| σs) <- traverse desugar bs
      foldM maybeJoin σ σs

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
