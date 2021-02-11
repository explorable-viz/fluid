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
import Data.Tuple (fst, snd)
import Bindings (Binding, Bindings, (↦), fromList)
import DataType (Ctr, DataType'(..), checkArity, checkDataType, ctrToDataType, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), Module(..), VarDef(..)) as E
import SExpr (
   Clause, Expr(..), ListPatternRest(..), ListRest(..), Module(..), Pattern(..), VarDefs, VarDef(..),
   RecDefs, Qualifier(..), RawExpr(..)
)
import Lattice (𝔹, (∧))
import Util (MayFail, error, type (×), (×), (≞), absurd, fromJust, mustLookup, report)


enil :: 𝔹 -> E.Expr 𝔹
enil α = E.Constr α cNil Nil

econs :: 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹 -> E.Expr 𝔹
econs α e e' = E.Constr α cCons (e : e' : Nil)

class DesugarFwd a b | a -> b where
   desugarFwd :: a -> MayFail b

-- Surface language supports "blocks" of variable declarations; core does not.
-- No need to pass "α = true" because desugarFwd is called on VarDef, not VarDefs?
instance desugarFwdModule :: DesugarFwd (Module Boolean) (E.Module Boolean) where
   desugarFwd (Module ds) = E.Module <$> traverse desugarFwd (join $ (ds <#> desugarDefs))
      where
      desugarDefs :: Either (VarDefs Boolean) (RecDefs Boolean)
                  -> List (Either (VarDef Boolean) (RecDefs Boolean))
      desugarDefs (Left ds')  = (toList ds' <#> Left)
      desugarDefs (Right δ)   = pure $ Right δ

{- p, e ↗ σ      specialisation of    p, κ ↗ σ -}
-- data E.VarDef a = E.VarDef (Elim a) (E.Expr a) -- where elim has codomain unit
instance desugarFwdVarDef :: DesugarFwd (VarDef Boolean) (E.VarDef Boolean) where
   desugarFwd (VarDef π s) = E.VarDef <$> desugarFwd (π × (None :: Cont 𝔹)) <*> desugarFwd s

{-        →                  -}
{- (α, (p , s), s_body) ↗ e  -}
-- | The first boolean represents the α of the outer expression which contains the var defs
instance desugarFwdVarDefs :: DesugarFwd (Boolean × NonEmptyList (VarDef Boolean) × Expr Boolean)
                                         (E.Expr Boolean) where
   desugarFwd (α1 × NonEmptyList (d@(VarDef _ (Expr α2 t)) :| Nil) × s) =
      E.Let <$> desugarFwd d <*> desugarFwd s
   desugarFwd (α1 × NonEmptyList (d@(VarDef _ (Expr α2 t)) :| d' : ds) × s) =
      E.Let <$> desugarFwd d <*> desugarFwd ((α1 ∧ α2) × NonEmptyList (d' :| ds) × s)
   desugarFwd (_ × NonEmptyList (VarDef _ Hole :| _) × _) =
      error "Encountered hole during desugar fwd"

{-       →                      →                 -}
{- let f c ↗ [f ↦ σ]       (f, (p, s))  ↗ [f ↦ σ] -}
instance desugarFwdRecDefs :: DesugarFwd (NonEmptyList (String × (NonEmptyList Pattern × Expr Boolean)))
                                         (Bindings Elim Boolean) where
   desugarFwd fπs = fromList <$> toList <$> reverse <$> traverse toRecDef fπss
      where
      fπss = groupBy (eq `on` fst) fπs :: NonEmptyList (NonEmptyList (Clause 𝔹))

      toRecDef :: NonEmptyList (Clause 𝔹) -> MayFail (Binding Elim 𝔹)
      toRecDef fπs' = ((↦) (fst $ head fπs')) <$> desugarFwd (snd <$> fπs')

{- s ↗ e -}
instance desugarFwdExpr :: DesugarFwd (Expr Boolean) (E.Expr Boolean) where
   desugarFwd Hole                           = error absurd
   desugarFwd (Expr _ (Var x))               = pure $ E.Var x
   desugarFwd (Expr _ (Op op))               = pure $ E.Op op
   desugarFwd (Expr α (Int n))               = pure $ E.Int α n
   desugarFwd (Expr α (Float n))             = pure $ E.Float α n
   desugarFwd (Expr α (Str s))               = pure $ E.Str α s
   desugarFwd (Expr α (Constr c ss))         = E.Constr α c <$> traverse desugarFwd ss
   desugarFwd (Expr α (Matrix s (x × y) s')) = E.Matrix α <$> desugarFwd s <@> x × y <*> desugarFwd s'
   desugarFwd (Expr _ (Lambda bs))           = E.Lambda <$> desugarFwd bs
   desugarFwd (Expr _ (App s1 s2))           = E.App <$> desugarFwd s1 <*> desugarFwd s2
   desugarFwd (Expr _ (BinaryApp s1 op s2))  = E.BinaryApp <$> desugarFwd s1 <@> op <*> desugarFwd s2
   desugarFwd (Expr _ (MatchAs s bs))        = E.App <$> (E.Lambda <$> desugarFwd bs) <*> desugarFwd s
   desugarFwd (Expr α (IfElse s1 s2 s3)) = do
      e2 <- desugarFwd s2
      e3 <- desugarFwd s3
      let σ = ElimConstr (fromFoldable [cTrue × Body e2, cFalse × Body e3])
      E.App (E.Lambda σ) <$> desugarFwd s1
   desugarFwd (Expr α (ListEmpty))           = pure $ enil α
   desugarFwd (Expr α (ListNonEmpty s l))    = econs α <$> desugarFwd s <*> desugarFwd l
   desugarFwd (Expr α (ListEnum s1 s2))      = E.App <$> ((E.App (E.Var "enumFromTo")) <$> desugarFwd s1) <*> desugarFwd s2
   -- | List-comp-done
   desugarFwd (Expr α1 (ListComp s_body (NonEmptyList (Guard _ (Expr α2 (Constr c Nil)) :| Nil)))) | c == cTrue = do
      e <- desugarFwd s_body
      pure $ econs (α1 ∧ α2) e (enil (α1 ∧ α2))
   -- | List-comp-qual-
   desugarFwd (Expr α (ListComp s_body (NonEmptyList (q :| Nil)))) =
      desugarFwd $ Expr α $ ListComp s_body $ NonEmptyList $ q :| (Guard α (Expr α $ Constr cTrue Nil)) : Nil
   -- | List-comp-guard
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList ((Guard α1 s) :| q : qs)))) = do
      e <- desugarFwd $ Expr α2 $ ListComp s_body $ NonEmptyList $ q :| qs
      let σ = ElimConstr (fromFoldable [cTrue × Body e, cFalse × Body (enil (α1 ∧ α2))])
      E.App (E.Lambda σ) <$> desugarFwd s
   -- | List-comp-decl
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList (Declaration α1 (VarDef π s) :| q : qs)))) = do
      e <- desugarFwd $ Expr α2 (ListComp s_body (NonEmptyList $ q :| qs))
      σ <- desugarFwd $ π × (Body e :: Cont 𝔹)
      E.App (E.Lambda σ) <$> desugarFwd s
   -- | List-comp-gen
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList ((Generator α1 p slist) :| q : qs)))) = do
      e <- desugarFwd $ Expr α2 $ ListComp s_body $ NonEmptyList $ q :| qs
      σ <- desugarFwd $ p × Body e
      E.App (E.App (E.Var "concatMap") (E.Lambda $ totalise σ (enil (α1 ∧ α2)))) <$> desugarFwd slist
   desugarFwd (Expr α2 (ListComp s_body (NonEmptyList (_ :| q : qs)))) = error "todo"
   desugarFwd (Expr α (Let ds s))            = desugarFwd $ α × ds × s
   -- | LetRec (recursive function)
   desugarFwd (Expr α (LetRec fπs s))        = E.LetRec <$> desugarFwd fπs <*> desugarFwd s

{- l ↗ e -}
instance desugarFwdListRest :: DesugarFwd (ListRest Boolean) (E.Expr Boolean) where
   desugarFwd (End α)       = pure (enil α)
   desugarFwd (Next α s l)  = lift2 (econs α) (desugarFwd s) (desugarFwd l)
   desugarFwd _             = error "Encountered a hole dering desugarFwdListRest"

{- →        -}
{- p, κ ↗ σ -}
instance desugarFwdPatternsCont :: DesugarFwd (NonEmptyList Pattern × Expr Boolean) (Elim Boolean) where
   desugarFwd (NonEmptyList (π :| Nil) × κ)     = desugarFwd $ π × κ
   desugarFwd (NonEmptyList (π :| π' : πs) × κ) = do
      κ' <- Body <$> E.Lambda <$> desugarFwd (NonEmptyList (π' :| πs) × κ)
      desugarFwd $ π × κ'

{- p, κ ↗ σ -}
-- Cont arguments here act as an accumulator.
instance desugarFwdPatternCont :: DesugarFwd (Pattern × Cont Boolean) (Elim Boolean) where
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

{- o, κ ↗ σ -}
instance desugarFwdListPatternRestCont :: DesugarFwd (ListPatternRest × Cont Boolean) (Elim Boolean) where
   desugarFwd (PEnd × κ)      = pure $ ElimConstr $ singleton cNil κ
   desugarFwd (PNext π o × κ) = do
      κ' <- Arg <$> desugarFwd (o × κ)
      ElimConstr <$> singleton cCons <$> Arg <$> desugarFwd (π × κ')

{-                →        -}
{- c ↗ σ   i.e.   p, s ↗ σ -}
instance desugarFwdBranchUncurried :: DesugarFwd (Pattern × Expr Boolean) (Elim Boolean) where
   desugarFwd (π × s) = do
      κ <- Body <$> desugarFwd s
      desugarFwd $ π × κ

-- To consolidate these without overlapping instances, probably need RecDefs to be a data type.
{- →     -}
{- c ↗ σ -}
instance desugarFwdBranches :: DesugarFwd (NonEmptyList (NonEmptyList Pattern × Expr Boolean)) (Elim Boolean) where
   desugarFwd bs = do
      NonEmptyList (σ :| σs) <- traverse desugarFwd bs
      foldM maybeJoin σ σs

instance desugarFwdBranchesUncurried :: DesugarFwd (NonEmptyList (Pattern × Expr Boolean)) (Elim Boolean) where
   desugarFwd bs = do
      NonEmptyList (σ :| σs) <- traverse desugarFwd bs
      foldM maybeJoin σ σs

instance desugarFwdEither :: (DesugarFwd a b, DesugarFwd c d) => DesugarFwd (Either a c) (Either b d) where
   desugarFwd (Left x) = Left <$> desugarFwd x
   desugarFwd (Right x) = Right <$> desugarFwd x

{- totalise κ ↗ κ'       totalise (singleton σ) enil = σ -}
totalise :: Elim 𝔹 -> E.Expr 𝔹 -> Elim 𝔹
totalise (ElimConstr m) e =
   let c × κ            = fromJust absurd $ L.head $ toUnfoldable m
       bs               = toUnfoldable m
       DataType _ sigs  = mustLookup c ctrToDataType
       bs'              = (_ × Body e) <$> ((fst <$> toUnfoldable sigs) \\ (fst <$> bs))
       bs''             = bs <#> \(c × κ) -> case mustLookup c m of
                           Arg σ   -> c × Arg (totalise σ e)
                           Body e' -> c × Body e'
                           None    -> c × Body e -- should the None cases should be undefined instead?
     in   ElimConstr $ fromFoldable $ bs'' <> bs'
totalise (ElimVar e κ) e' = case κ of
   Arg σ  -> ElimVar e $ Arg $ totalise σ e'
   Body _ -> ElimVar e κ
   None   -> ElimVar e $ Body e'

class Joinable a where
   maybeJoin :: a -> a -> MayFail a

instance joinableElim :: Joinable (Elim Boolean) where
   maybeJoin (ElimVar x κ) (ElimVar y κ')       = ElimVar <$> x ≞ y <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeJoin κs κs'
   maybeJoin _ _                                = report "Can't join variable and constructor patterns"

instance joinableCont :: Joinable (Cont Boolean) where
   maybeJoin None None                                = pure None
   maybeJoin (Arg σ) (Arg σ')                         = Arg <$> maybeJoin σ σ'
   maybeJoin (Body (E.Lambda σ)) (Body (E.Lambda σ')) = Body <$> (E.Lambda <$> maybeJoin σ σ')
   maybeJoin _ _                                      = report "Incompatible continuations"

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
