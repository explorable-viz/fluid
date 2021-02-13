module DesugarBwd where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Data.Function (on)
import Data.Either (Either(..))
import Data.List (List(..), (:), zip)
import Data.List.NonEmpty (NonEmptyList(..), groupBy, toList, reverse)
import Data.Map (fromFoldable)
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (uncurry, fst, snd)
import Bindings (Binding, Bindings(..), (↦), (:+:))
import DataType (cPair, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), VarDef(..)) as E
import Pretty (render, pretty)
import SExpr (Clause, Expr(..), ListRest(..), Pattern(..), ListPatternRest(..), Qualifier(..), VarDef(..))
import Lattice (𝔹, (∧))
import Util (MayFail, type(+), type (×), (×), (≞), (≜), absurd, assert, mustLookup, lookupE, error)

qualTrue :: 𝔹 -> Qualifier 𝔹
qualTrue α = Guard α (Constr α cTrue Nil)

snil :: 𝔹 -> Expr 𝔹
snil α = Constr α cNil Nil

class DesugarBwd a b where
   desugarBwd :: a -> b -> MayFail b

instance desugarBwdVarDef  :: DesugarBwd (E.VarDef Boolean) (VarDef Boolean) where
   desugarBwd (E.VarDef σ e) (VarDef π s) = VarDef π <$> desugarBwd e s

instance desugarBwdVarDefs :: DesugarBwd (E.Expr Boolean)
                                         (NonEmptyList (VarDef Boolean) × Expr Boolean) where
   desugarBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) = do
      s1' <- desugarBwd e1 s1
      (NonEmptyList (VarDef π s1' :| Nil) × _) <$> desugarBwd e2 s2
   desugarBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) = do
      s1' <- desugarBwd e1 s1
      NonEmptyList (d' :| ds') × s2' <- desugarBwd e2 (NonEmptyList (d :| ds) × s2)
      pure $ NonEmptyList (VarDef π s1' :| d' : ds') × s2'
   desugarBwd _ _ = error absurd

instance desugarBwdRecDefs ::
         DesugarBwd (Bindings Elim Boolean) (NonEmptyList (String × (NonEmptyList Pattern × Expr Boolean))) where
   desugarBwd fσs fπes = join <$> zipRecDefs fσs fπess
      where
      fπess = reverse (groupBy (eq `on` fst) fπes :: NonEmptyList (NonEmptyList (Clause 𝔹)))

      zipRecDefs :: Bindings Elim 𝔹 ->
                    NonEmptyList (NonEmptyList (Clause 𝔹)) ->
                    MayFail (NonEmptyList (NonEmptyList (Clause 𝔹)))
      zipRecDefs (ρ :+: f ↦ σ) (NonEmptyList (fπes1 :| fπes2 : fπes_rest)) = do
         fπes1' <- fromRecDef (f ↦ σ) fπes1
         fπess' <- toList <$> zipRecDefs ρ (NonEmptyList (fπes2 :| fπes_rest))
         pure $ NonEmptyList (fπes1' :| fπess')
      zipRecDefs (Empty :+: f ↦ σ) (NonEmptyList (fπes1 :| Nil)) = do
         fπes1' <- fromRecDef (f ↦ σ) fπes1
         pure $ NonEmptyList (fπes1' :| Nil)
      zipRecDefs ρ fπs = error absurd

      fromRecDef :: Binding Elim 𝔹 -> NonEmptyList (Clause 𝔹) -> MayFail (NonEmptyList (Clause 𝔹))
      fromRecDef (f ↦ σ) fπs@(NonEmptyList ((f' × (πs × e)) :| fπs')) =
         map ((×) f) <$> desugarBwd σ (snd <$> fπs)

instance desugarBwdExpr :: DesugarBwd (E.Expr Boolean) (Expr Boolean) where
   desugarBwd (E.Var x)             (Var x')          = pure $ Var (x ≜ x')
   desugarBwd (E.Op op)             (Op op')          = pure $ Op (op ≜ op')
   desugarBwd (E.Int α n)           (Int _ n')        = pure $ Int α (n ≜ n')
   desugarBwd (E.Float α n)         (Float _ n')      = pure $ Float α (n ≜ n')
   desugarBwd (E.Str α s)           (Str _ s')        = pure $ Str α (s ≜ s')
   desugarBwd (E.Constr α c es)     (Constr _ c' es') =
      Constr α (c ≜ c') <$> traverse (uncurry desugarBwd) (zip es es')
   desugarBwd (E.Matrix α e (x × y) e') (Matrix _ s (x' × y') s') =
      Matrix α <$> desugarBwd e s <@> (x ≜ x') × (y ≜ y') <*> desugarBwd e' s'
   desugarBwd (E.Lambda σ)          (Lambda bs)       = Lambda <$> desugarBwd σ bs
   desugarBwd (E.App e1 e2)         (App s1 s2)       = App <$> desugarBwd e1 s1 <*> desugarBwd e2 s2
   desugarBwd (E.App (E.Lambda σ) e) (MatchAs s bs)   = MatchAs <$> desugarBwd e s <*> desugarBwd σ bs
   desugarBwd (E.App (E.Lambda (ElimConstr m)) e1) (IfElse s1 s2 s3) = do
      e2 <- asExpr <$> lookupE cTrue m
      e3 <- asExpr <$> lookupE cFalse m
      IfElse <$> desugarBwd e1 s1 <*> desugarBwd e2 s2 <*> desugarBwd e3 s3
   desugarBwd (E.BinaryApp e1 x e2) (BinaryApp s1 x' s2) =
      BinaryApp <$> desugarBwd e1 s1 <@> x ≜ x' <*> desugarBwd e2 s2
   desugarBwd (E.Constr α c Nil)    (ListEmpty _) | c == cNil =
      pure $ ListEmpty α
   desugarBwd (E.Constr α c (e : e' : Nil)) (ListNonEmpty _ s l) | c == cCons =
      ListNonEmpty α <$> desugarBwd e s <*> desugarBwd e' l
   -- | List-enum
   desugarBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
      ListEnum <$> desugarBwd e1 s1 <*> desugarBwd e2 s2
   -- | List-comp-done
   desugarBwd (E.Constr α2 c (e : (E.Constr α1 c' Nil) : Nil))
              (ListComp _ s_body (NonEmptyList (Guard _ (Constr _ c'' Nil) :| Nil)))
      | c == cCons , c' == cNil, c'' == cTrue =
      ListComp (α1 ∧ α2) <$> desugarBwd e s_body
                         <*> pure (NonEmptyList (Guard (α1 ∧ α2) (Constr (α1 ∧ α2) cTrue Nil) :| Nil))
   -- | List-comp-qual
   desugarBwd e (ListComp α s_body (NonEmptyList (q :| Nil))) = do
      sListComp <- desugarBwd e (ListComp α s_body (NonEmptyList (q :| qualTrue true : Nil)))
      case sListComp of
         ListComp α2 s_body' (NonEmptyList (q' :| (Guard α1 (Constr _ c Nil)) : Nil))
         | c == cTrue
            -> pure $ ListComp (α1 ∧ α2) s_body' (NonEmptyList (q' :| Nil))
         sListComp'
            -> error $ "desugarBwd for List-comp-qual failed: \n" <>
                       render (pretty sListComp')
   -- | List-comp-guard
   desugarBwd (E.App (E.Lambda (ElimConstr m)) e1)
              (ListComp α s1 (NonEmptyList (Guard _ s2 :| q : qs))) = do
      e2          <- asExpr <$> lookupE cTrue  m
      e3          <- asExpr <$> lookupE cFalse m
      s2'         <- desugarBwd e1 s2
      sListComp   <- desugarBwd e2 (ListComp α s1 (NonEmptyList (q :| qs)))
      sNil        <- desugarBwd e3 (snil true)
      case sListComp, sNil of
         ListComp α3 s1' (NonEmptyList (q' :| qs')), Constr α4 c Nil | c == cNil ->
            pure $ ListComp (α3 ∧ α4) s1' (NonEmptyList (Guard (α3 ∧ α4) s2' :| q' : qs'))
         _, _ -> error absurd
   -- | List-comp-decl
   desugarBwd (E.App (E.Lambda σ) e)
              (ListComp α s2 (NonEmptyList ((Declaration _ (VarDef π s1)) :| q : qs))) = do
      (_ × sListComp)  <- desugarBwd σ (NonEmptyList (π :| Nil) × (ListComp α s2 (NonEmptyList (q :| qs))))
      s1'  <- desugarBwd e s1
      case sListComp of
         ListComp α3 s2' (NonEmptyList (q' :| qs')) ->
            pure $ ListComp α3 s2' (NonEmptyList ((Declaration α3 (VarDef π s1')) :| q' : qs'))
         _ -> error absurd
   -- | List-comp-gen
   desugarBwd (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1)
              (ListComp α s2 (NonEmptyList (Generator _ p s1 :| q : qs))) = do
      s1'        <- desugarBwd e1 s1
      let σ' = asElim (untotalise (Arg σ) (Left p : Nil))
      e2         <- asExpr <$> desugarPatternBwd σ' p
      sListComp  <- desugarBwd e2 (ListComp α s2 (NonEmptyList (q :| qs)))
      case sListComp of
         ListComp α4 s2' (NonEmptyList (q' :| qs')) ->
            pure $ ListComp α4 s2' (NonEmptyList (Generator α4 p s1 :| q' : qs'))
         _ -> error absurd
   -- | Let
   desugarBwd (E.Let d e) (Let ds s) = do
      ds' × s' <- desugarBwd (E.Let d e) (ds × s)
      pure $ Let ds' s'
   -- | LetRec (recursive function)
   desugarBwd (E.LetRec fπs e) (LetRec fπs' s) = LetRec <$> desugarBwd fπs fπs' <*> desugarBwd e s
   desugarBwd (E.Hole) s = error "todo"

   desugarBwd e s = error $ "desugarBwd match not found: " <> render (pretty e) <> "\n" <> render (pretty s)

{- e, l ↘ l -}
instance desugarBwdListRest :: DesugarBwd (E.Expr Boolean) (ListRest Boolean) where
   desugarBwd (E.Constr α c Nil) (End _) | c == cNil =
      pure $ End α
   desugarBwd (E.Constr α c (e : e' : Nil)) (Next _ s l) | c == cCons =
      Next α <$> desugarBwd e s <*> desugarBwd e' l
   desugarBwd (E.Hole) s = error "todo"
   desugarBwd e l = error $ "desugarBwdListRest (e, l) match not found: \n" <>
                            render (pretty e) <> "\n" <>
                            render (pretty l)

class DesugarPatternBwd a where
   desugarPatternBwd :: Elim Boolean -> a -> MayFail (Cont Boolean)

{-    →     -}
{- σ, p ↘ κ -}
instance desugarPatternBwdPatterns :: DesugarPatternBwd (NonEmptyList Pattern) where
   desugarPatternBwd σ (NonEmptyList (π :| Nil)) = desugarPatternBwd σ π
   desugarPatternBwd σ (NonEmptyList (π :| π' : πs)) = do
      test <- desugarPatternBwd σ π
      σ' <- asElim <$> desugarPatternBwd σ π
      desugarPatternBwd σ' (NonEmptyList (π' :| πs))

{- σ, p ↘ κ -}
instance desugarPatternBwdPattern :: DesugarPatternBwd Pattern where
   desugarPatternBwd (ElimVar x κ)  (PVar x') = (x ≞ x') *> pure κ
   desugarPatternBwd (ElimConstr m) (PConstr c Nil) | c == cTrue = lookupE cTrue m
   desugarPatternBwd (ElimConstr m) (PConstr c Nil) | c == cFalse = lookupE cFalse m
   desugarPatternBwd (ElimConstr m) (PConstr c Nil) | c == cNil = lookupE cNil m
   desugarPatternBwd (ElimConstr m) (PConstr c (π : π' : _)) | c == cCons || c == cPair = do
      σ  <- asElim <$> lookupE c m
      σ' <- asElim <$> desugarPatternBwd σ π
      desugarPatternBwd σ' π'
   desugarPatternBwd (ElimConstr m) (PListEmpty) = lookupE cNil m
   desugarPatternBwd σ (PListNonEmpty π o)  = do
      σ' <- asElim <$> desugarPatternBwd σ π
      desugarPatternBwd σ' o
   desugarPatternBwd σ π = error absurd

{- σ, o ↘ κ -}
instance desugarPatternBwdListPatternRest :: DesugarPatternBwd ListPatternRest where
   desugarPatternBwd (ElimConstr m) PEnd        = lookupE cCons m
   desugarPatternBwd (ElimConstr m) (PNext π o) = do
      σ  <- asElim <$> lookupE cCons m
      σ' <- asElim <$> desugarPatternBwd σ π
      desugarPatternBwd σ' o
   desugarPatternBwd σ l = error $ "desugarPatternBwdListPatternRest (σ, l) match not found: \n" <>
                                   render (pretty σ) <> "\n" <>
                                   render (pretty l)
{- σ, c ↘ c -}
instance desugarBwdBranch :: DesugarBwd (Elim Boolean) (NonEmptyList Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e <- asExpr <$> desugarPatternBwd σ πs
      (πs × _) <$> desugarBwd e s

instance desugarBwdBranchUncurried :: DesugarBwd (Elim Boolean) (Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e <- asExpr <$> desugarPatternBwd σ πs
      (πs × _) <$> desugarBwd e s

{- σ, cs ↘ c -}
instance desugarBwdBranches :: DesugarBwd (Elim Boolean) (NonEmptyList (NonEmptyList Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) =
      NonEmptyList <$> (desugarBwd σ b1 `lift2 (:|)` (toList <$> desugarBwd σ (NonEmptyList (b2 :| bs))))
   desugarBwd σ (NonEmptyList (b :| Nil)) =
      NonEmptyList <$> (desugarBwd σ b `lift2 (:|)` pure Nil)

instance desugarBwdBranchesUncurried :: DesugarBwd (Elim Boolean) (NonEmptyList (Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) =
      NonEmptyList <$> (desugarBwd σ b1 `lift2 (:|)` (toList <$> desugarBwd σ (NonEmptyList (b2 :| bs))))
   desugarBwd σ (NonEmptyList (b :| Nil)) =
      NonEmptyList <$> (desugarBwd σ b `lift2 (:|)` pure Nil)

{- untotalise κ πs ↗ κ' -}
untotalise :: Cont 𝔹 -> List (Pattern + ListPatternRest) -> Cont 𝔹
untotalise κ Nil = κ
untotalise None (_ : _) = error "todo" -- is the None case essentially Hole?
untotalise (Arg σ) (π : πs) =
   case σ × π of
      ElimVar x κ × Left (PVar x') ->
         assert (x == x') $
         Arg (ElimVar x (untotalise κ πs))
      ElimVar _ _ × _ ->
         error absurd
      ElimConstr _ × Left (PVar _) ->
         error absurd
      ElimConstr m × Left (PConstr c ps) ->
         let κ' = untotalise (mustLookup c m) (map Left ps <> πs)
         in Arg (ElimConstr (fromFoldable [c × κ']))
      ElimConstr m × Left PListEmpty ->
         let κ' = untotalise (mustLookup cNil m) πs
         in Arg (ElimConstr (fromFoldable [cNil × κ']))
      ElimConstr m × Left (PListNonEmpty p o) ->
         let κ' = untotalise (mustLookup cCons m) (Left p : Right o : πs)
         in Arg (ElimConstr (fromFoldable [cCons × κ']))
      ElimConstr m × Right PEnd ->
         let κ' = untotalise (mustLookup cNil m) πs
         in Arg (ElimConstr (fromFoldable [cNil × κ']))
      ElimConstr m × Right (PNext p o) ->
         let κ' = untotalise (mustLookup cCons m) (Left p : Right o : πs)
         in Arg (ElimConstr (fromFoldable [cCons × κ']))
untotalise (Body _) (_ : _) = error absurd
