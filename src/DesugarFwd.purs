module DesugarFwd where

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
import Data.Tuple (fst, snd)
import Bindings (Binding, Bindings, (↦), fromList)
import DataType (Ctr, DataType'(..), checkArity, checkDataType, ctrToDataType, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), asElim)
import Expr (Expr(..), Module(..), VarDef(..)) as E
import Lattice (𝔹)
import SExpr (
   Clause, Expr(..), ListPatternRest(..), ListRest(..), Module(..), Pattern(..), VarDefs, VarDef(..), RecDefs, Qualifier(..)
)
import Util (MayFail, type (+), type (×), (×), (≞), absurd, error, fromJust, mustLookup, report)

enil :: 𝔹 -> E.Expr 𝔹
enil α = E.Constr α cNil Nil

econs :: 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹
econs α e e' = E.Constr α cCons (e : e' : Nil)

-- "Vanilla" desugaring is just forward-slicing where we disregard annotations, so user errors may occur during
-- forward slicing.
class DesugarFwd a b | a -> b where
   desugarFwd :: a -> MayFail b

-- Surface language supports "blocks" of variable declarations; core does not.
instance module_ :: DesugarFwd (Module Boolean) (E.Module Boolean) where
   desugarFwd (Module ds) = E.Module <$> traverse desugarFwd (join $ (ds <#> desugarDefs))
      where
      desugarDefs :: VarDefs Boolean + RecDefs Boolean -> List (VarDef Boolean + RecDefs Boolean)
      desugarDefs (Left ds')  = toList ds' <#> Left
      desugarDefs (Right δ)   = pure $ Right δ

instance varDef :: DesugarFwd (VarDef Boolean) (E.VarDef Boolean) where
   desugarFwd (VarDef π s) = E.VarDef <$> desugarFwd (π × (ContHole :: Cont 𝔹)) <*> desugarFwd s

instance varDefs :: DesugarFwd (NonEmptyList (VarDef Boolean) × Expr Boolean) (E.Expr Boolean) where
   desugarFwd (NonEmptyList (d :| Nil) × s) =
      E.Let <$> desugarFwd d <*> desugarFwd s
   desugarFwd (NonEmptyList (d :| d' : ds) × s) =
      E.Let <$> desugarFwd d <*> desugarFwd (NonEmptyList (d' :| ds) × s)

instance recDefs :: DesugarFwd (NonEmptyList (String × (NonEmptyList Pattern × Expr Boolean))) (Bindings Elim Boolean) where
   desugarFwd fπs = fromList <$> toList <$> reverse <$> traverse toRecDef fπss
      where
      fπss = groupBy (eq `on` fst) fπs :: NonEmptyList (NonEmptyList (Clause 𝔹))

      toRecDef :: NonEmptyList (Clause 𝔹) -> MayFail (Binding Elim 𝔹)
      toRecDef fπs' = ((↦) (fst (head fπs'))) <$> desugarFwd (snd <$> fπs')

instance expr :: DesugarFwd (Expr Boolean) (E.Expr Boolean) where
   desugarFwd (Var x)                  = pure (E.Var x)
   desugarFwd (Op op)                  = pure (E.Op op)
   desugarFwd (Int α n)                = pure (E.Int α n)
   desugarFwd (Float α n)              = pure (E.Float α n)
   desugarFwd (Str α s)                = pure (E.Str α s)
   desugarFwd (Constr α c ss)          = E.Constr α c <$> traverse desugarFwd ss
   desugarFwd (Matrix α s (x × y) s')  = E.Matrix α <$> desugarFwd s <@> x × y <*> desugarFwd s'
   desugarFwd (Lambda bs)              = E.Lambda <$> desugarFwd bs
   desugarFwd (App s1 s2)              = E.App <$> desugarFwd s1 <*> desugarFwd s2
   desugarFwd (BinaryApp s1 op s2)     = E.BinaryApp <$> desugarFwd s1 <@> op <*> desugarFwd s2
   desugarFwd (MatchAs s bs)           = E.App <$> (E.Lambda <$> desugarFwd bs) <*> desugarFwd s
   desugarFwd (IfElse s1 s2 s3) = do
      e2 <- desugarFwd s2
      e3 <- desugarFwd s3
      let σ = ElimConstr (fromFoldable [cTrue × ContExpr e2, cFalse × ContExpr e3])
      E.App (E.Lambda σ) <$> desugarFwd s1
   desugarFwd (ListEmpty α)            = pure (enil α)
   desugarFwd (ListNonEmpty α s l)     = econs α <$> desugarFwd s <*> desugarFwd l
   desugarFwd (ListEnum s1 s2)         = E.App <$> ((E.App (E.Var "enumFromTo")) <$> desugarFwd s1) <*> desugarFwd s2
   -- | List-comp-done
   desugarFwd (ListComp α s_body (NonEmptyList (Guard (Constr α2 c Nil) :| Nil))) | c == cTrue = do
      econs α2 <$> desugarFwd s_body <@> enil α2
   -- | List-comp-last
   desugarFwd (ListComp α s_body (NonEmptyList (q :| Nil))) =
      desugarFwd (ListComp α s_body (NonEmptyList (q :| Guard (Constr α cTrue Nil) : Nil)))
   -- | List-comp-guard
   desugarFwd (ListComp α s_body (NonEmptyList (Guard s :| q : qs))) = do
      e <- desugarFwd (ListComp α s_body (NonEmptyList (q :| qs)))
      let σ = ElimConstr (fromFoldable [cTrue × ContExpr e, cFalse × ContExpr (enil α)])
      E.App (E.Lambda σ) <$> desugarFwd s
   -- | List-comp-decl
   desugarFwd (ListComp α s_body (NonEmptyList (Declaration (VarDef π s) :| q : qs))) = do
      e <- desugarFwd (ListComp α s_body (NonEmptyList (q :| qs)))
      σ <- desugarFwd (π × (ContExpr e :: Cont 𝔹))
      E.App (E.Lambda σ) <$> desugarFwd s
   -- | List-comp-gen
   desugarFwd (ListComp α s_body (NonEmptyList (Generator p slist :| q : qs))) = do
      e <- desugarFwd (ListComp α s_body (NonEmptyList (q :| qs)))
      σ <- desugarFwd (p × ContExpr e)
      E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (totalise (ContElim σ) α)))) <$> desugarFwd slist
   desugarFwd (Let ds s)               = desugarFwd (ds × s)
   desugarFwd (LetRec fπs s)           = E.LetRec <$> desugarFwd fπs <*> desugarFwd s

instance listRest :: DesugarFwd (ListRest Boolean) (E.Expr Boolean) where
   desugarFwd (End α)       = pure (enil α)
   desugarFwd (Next α s l)  = econs α <$> desugarFwd s <*> desugarFwd l

instance patternsExpr :: DesugarFwd (NonEmptyList Pattern × Expr Boolean) (Elim Boolean) where
   desugarFwd (NonEmptyList (p :| Nil) × e) = desugarFwd (p × e)
   desugarFwd (NonEmptyList (p :| p' : ps) × e) =
      (desugarFwd <<< (p × _)) =<< ContExpr <$> E.Lambda <$> desugarFwd (NonEmptyList (p' :| ps) × e)

-- Cont argument here acts as an accumulator.
instance patternCont :: DesugarFwd (Pattern × Cont Boolean) (Elim Boolean) where
   desugarFwd (PVar x × κ) = pure (ElimVar x κ)
   desugarFwd (PConstr c πs × κ) = checkArity c (length πs) *> (ElimConstr <$> singleton c <$> toCont πs)
      where
      toCont :: List Pattern -> MayFail (Cont 𝔹)
      toCont Nil = pure κ
      toCont (π : πs') = ContElim <$> (desugarFwd <<< (π × _) =<< toCont πs')
   desugarFwd (PListEmpty × κ) = pure (ElimConstr (singleton cNil κ))
   desugarFwd (PListNonEmpty π o × κ)  = do
      κ' <- ContElim <$> desugarFwd (o × κ)
      ElimConstr <$> singleton cCons <$> ContElim <$> desugarFwd (π × κ')

instance listPatternRestCont :: DesugarFwd (ListPatternRest × Cont Boolean) (Elim Boolean) where
   desugarFwd (PEnd × κ) = pure (ElimConstr (singleton cNil κ))
   desugarFwd (PNext π o × κ) = do
      κ' <- ContElim <$> desugarFwd (o × κ)
      ElimConstr <$> singleton cCons <$> ContElim <$> desugarFwd (π × κ')

instance branchUncurried :: DesugarFwd (Pattern × Expr Boolean) (Elim Boolean) where
   desugarFwd (π × s) = (ContExpr <$> desugarFwd s) >>= (desugarFwd <<< (π × _))

-- To consolidate these without overlapping instances, probably need RecDefs to be a data type.
instance branches :: DesugarFwd (NonEmptyList (NonEmptyList Pattern × Expr Boolean)) (Elim Boolean) where
   desugarFwd bs = do
      NonEmptyList (σ :| σs) <- traverse desugarFwd bs
      foldM maybeJoin σ σs

instance branchesUncurried :: DesugarFwd (NonEmptyList (Pattern × Expr Boolean)) (Elim Boolean) where
   desugarFwd bs = do
      NonEmptyList (σ :| σs) <- traverse desugarFwd bs
      foldM maybeJoin σ σs

instance either :: (DesugarFwd a b, DesugarFwd c d) => DesugarFwd (a + c) (b + d) where
   desugarFwd (Left x) = Left <$> desugarFwd x
   desugarFwd (Right x) = Right <$> desugarFwd x

-- holes used to represent var defs, but otherwise surface programs never contain holes
totalise :: Cont 𝔹 -> 𝔹 -> Cont 𝔹
totalise ContHole _                    = error absurd
totalise (ContExpr e) _                = ContExpr e
totalise (ContElim ElimHole) _         = error absurd
totalise (ContElim (ElimConstr m)) α   =
   let c × κ = fromJust absurd (L.head (toUnfoldable m))
       bs = toUnfoldable m
       DataType _ sigs = mustLookup c ctrToDataType
       bs' = (_ × ContExpr (enil α)) <$> ((fst <$> toUnfoldable sigs) \\ (fst <$> bs))
   in ContElim (ElimConstr (fromFoldable ((bs <#> \(c × κ) -> c × totalise κ α) <> bs')))
totalise (ContElim (ElimVar x κ)) α    = ContElim (ElimVar x (totalise κ α))

-- TODO: subsume with (or distinguish from) Lattice instance on Elim
class Joinable a where
   maybeJoin :: a -> a -> MayFail a

instance joinableElim :: Joinable (Elim Boolean) where
   maybeJoin (ElimVar x κ) (ElimVar y κ')       = ElimVar <$> x ≞ y <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeJoin κs κs'
   maybeJoin _ _                                = report "Unmergable function branches"

instance joinableCont :: Joinable (Cont Boolean) where
   maybeJoin (ContElim σ) (ContElim σ')                        = ContElim <$> maybeJoin σ σ'
   maybeJoin (ContExpr (E.Lambda σ)) (ContExpr (E.Lambda σ'))  = ContExpr <$> (E.Lambda <$> maybeJoin σ σ')
   maybeJoin _ _                                               = report "Unmergable function branches"

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
