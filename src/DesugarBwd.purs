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
import DataType (cCons, cNil, cTrue, cFalse)
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

instance varDef  :: DesugarBwd (E.VarDef Boolean) (VarDef Boolean) where
   desugarBwd (E.VarDef σ e) (VarDef π s) = VarDef π <$> desugarBwd e s

instance varDefs :: DesugarBwd (E.Expr Boolean) (NonEmptyList (VarDef Boolean) × Expr Boolean) where
   desugarBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) = do
      s1' <- desugarBwd e1 s1
      (NonEmptyList (VarDef π s1' :| Nil) × _) <$> desugarBwd e2 s2
   desugarBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) = do
      s1' <- desugarBwd e1 s1
      NonEmptyList (d' :| ds') × s2' <- desugarBwd e2 (NonEmptyList (d :| ds) × s2)
      pure $ NonEmptyList (VarDef π s1' :| d' : ds') × s2'
   desugarBwd _ _ = error absurd

instance recDefs :: DesugarBwd (Bindings Elim Boolean) (NonEmptyList (String × (NonEmptyList Pattern × Expr Boolean))) where
   desugarBwd xσs xcs = join <$> zipRecDefs xσs xcss
      where
      xcss = reverse (groupBy (eq `on` fst) xcs :: NonEmptyList (NonEmptyList (Clause 𝔹)))

zipRecDefs :: Bindings Elim 𝔹 ->
              NonEmptyList (NonEmptyList (Clause 𝔹)) ->
              MayFail (NonEmptyList (NonEmptyList (Clause 𝔹)))
zipRecDefs Empty _ = error absurd
zipRecDefs (Empty :+: x ↦ σ) (NonEmptyList (xcs :| Nil)) = do
   NonEmptyList <$> (fromRecDef (x ↦ σ) xcs `lift2 (:|)` pure Nil)
zipRecDefs (_ :+: _ :+: _) (NonEmptyList (_ :| Nil)) = error absurd
zipRecDefs (ρ :+: x ↦ σ) (NonEmptyList (xcs1 :| xcs2 : xcss)) = do
   NonEmptyList <$> (fromRecDef (x ↦ σ) xcs1 `lift2 (:|)` (toList <$> zipRecDefs ρ (NonEmptyList (xcs2 :| xcss))))

fromRecDef :: Binding Elim 𝔹 -> NonEmptyList (Clause 𝔹) -> MayFail (NonEmptyList (Clause 𝔹))
fromRecDef (x ↦ σ) xcs = map (x × _) <$> desugarBwd σ (snd <$> xcs)

instance expr :: DesugarBwd (E.Expr Boolean) (Expr Boolean) where
   desugarBwd (E.Var x) (Var x')          = pure $ Var (x ≜ x')
   desugarBwd (E.Op op) (Op op')          = pure $ Op (op ≜ op')
   desugarBwd (E.Int α n) (Int _ n')      = pure $ Int α (n ≜ n')
   desugarBwd (E.Float α n) (Float _ n')  = pure $ Float α (n ≜ n')
   desugarBwd (E.Str α s) (Str _ s')      = pure $ Str α (s ≜ s')
   desugarBwd (E.Constr α c es) (Constr _ c' es') =
      Constr α (c ≜ c') <$> traverse (uncurry desugarBwd) (zip es es')
   desugarBwd (E.Matrix α e (x × y) e') (Matrix _ s (x' × y') s') =
      Matrix α <$> desugarBwd e s <@> (x ≜ x') × (y ≜ y') <*> desugarBwd e' s'
   desugarBwd (E.Lambda σ) (Lambda bs) =
      Lambda <$> desugarBwd σ bs
   desugarBwd (E.App e1 e2) (App s1 s2) =
      App <$> desugarBwd e1 s1 <*> desugarBwd e2 s2
   desugarBwd (E.App (E.Lambda σ) e) (MatchAs s bs)   = MatchAs <$> desugarBwd e s <*> desugarBwd σ bs
   desugarBwd (E.App (E.Lambda (ElimConstr m)) e1) (IfElse s1 s2 s3) = do
      IfElse <$> desugarBwd e1 s1 <*>
                 desugarBwd (asExpr (mustLookup cTrue m)) s2 <*>
                 desugarBwd (asExpr (mustLookup cFalse m)) s3
   desugarBwd (E.BinaryApp e1 x e2) (BinaryApp s1 x' s2) =
      BinaryApp <$> desugarBwd e1 s1 <@> x ≜ x' <*> desugarBwd e2 s2
   desugarBwd (E.Constr α c Nil) (ListEmpty _) | c == cNil =
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
      e2 <- asExpr <$> lookupE cTrue  m
      e3 <- asExpr <$> lookupE cFalse m
      s2' <- desugarBwd e1 s2
      sListComp <- desugarBwd e2 (ListComp α s1 (NonEmptyList (q :| qs)))
      sNil <- desugarBwd e3 (snil true)
      case sListComp, sNil of
         ListComp α3 s1' (NonEmptyList (q' :| qs')), Constr α4 c Nil | c == cNil ->
            pure $ ListComp (α3 ∧ α4) s1' (NonEmptyList (Guard (α3 ∧ α4) s2' :| q' : qs'))
         _, _ -> error absurd
   -- | List-comp-decl
   desugarBwd (E.App (E.Lambda σ) e)
              (ListComp α s2 (NonEmptyList ((Declaration _ (VarDef π s1)) :| q : qs))) = do
      (_ × sListComp)  <- desugarBwd σ (NonEmptyList (π :| Nil) × (ListComp α s2 (NonEmptyList (q :| qs))))
      s1' <- desugarBwd e s1
      case sListComp of
         ListComp α3 s2' (NonEmptyList (q' :| qs')) ->
            pure $ ListComp α3 s2' (NonEmptyList ((Declaration α3 (VarDef π s1')) :| q' : qs'))
         _ -> error absurd
   -- | List-comp-gen
   desugarBwd (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1)
              (ListComp α s2 (NonEmptyList (Generator _ p s1 :| q : qs))) = do
      s1' <- desugarBwd e1 s1
      let σ' = asElim (untotalise (Arg σ) (Left p : Nil))
      e2 <- asExpr <$> desugarPatternBwd σ' p
      sListComp  <- desugarBwd e2 (ListComp α s2 (NonEmptyList (q :| qs)))
      case sListComp of
         ListComp α4 s2' (NonEmptyList (q' :| qs')) ->
            pure $ ListComp α4 s2' (NonEmptyList (Generator α4 p s1 :| q' : qs'))
         _ -> error absurd
   desugarBwd (E.Let d e) (Let ds s) = do
      ds' × s' <- desugarBwd (E.Let d e) (ds × s)
      pure $ Let ds' s'
   desugarBwd (E.LetRec fπs e) (LetRec fπs' s) = LetRec <$> desugarBwd fπs fπs' <*> desugarBwd e s
   desugarBwd (E.Hole) s = error "todo"
   desugarBwd _ _ = error absurd

{- e, l ↘ l -}
instance listRest :: DesugarBwd (E.Expr Boolean) (ListRest Boolean) where
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

{- σ, ps ↘ κ -}
instance patterns :: DesugarPatternBwd (NonEmptyList Pattern) where
   desugarPatternBwd σ (NonEmptyList (π :| Nil)) = desugarPatternBwd σ π
   desugarPatternBwd σ (NonEmptyList (π :| π' : πs)) = do
      test <- desugarPatternBwd σ π
      σ' <- asElim <$> desugarPatternBwd σ π
      desugarPatternBwd σ' (NonEmptyList (π' :| πs))

{- σ, p ↘ κ -}
instance pattern :: DesugarPatternBwd Pattern where
   -- TODO: hole cases
   desugarPatternBwd (ElimVar x κ) (PVar x') = (x ≞ x') *> pure κ
   desugarPatternBwd (ElimConstr _) (PVar _) = error absurd

   desugarPatternBwd (ElimVar _ _) (PConstr c _) = error absurd
   desugarPatternBwd (ElimConstr m) (PConstr c Nil) = pure (mustLookup c m)
   desugarPatternBwd (ElimConstr m) (PConstr c (π : πs)) = do
      desugarPatternBwd (asElim (mustLookup c m)) (NonEmptyList (π :| πs))

   desugarPatternBwd (ElimVar _ _) (PListEmpty) = error absurd
   desugarPatternBwd (ElimConstr m) (PListEmpty) = pure (mustLookup cNil m)

   desugarPatternBwd σ (PListNonEmpty π o) = do
      σ' <- asElim <$> desugarPatternBwd σ π
      desugarPatternBwd σ' o

{- σ, o ↘ κ -}
instance patternRest :: DesugarPatternBwd ListPatternRest where
   desugarPatternBwd (ElimVar _ _) _ = error absurd
   desugarPatternBwd (ElimConstr m) PEnd = pure (mustLookup cCons m)
   desugarPatternBwd (ElimConstr m) (PNext π o) = do
      σ' <- asElim <$> desugarPatternBwd (asElim (mustLookup cCons m)) π
      desugarPatternBwd σ' o

{- σ, c ↘ c -}
instance branch :: DesugarBwd (Elim Boolean) (NonEmptyList Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e <- asExpr <$> desugarPatternBwd σ πs
      (πs × _) <$> desugarBwd e s

instance branchUncurried :: DesugarBwd (Elim Boolean) (Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e <- asExpr <$> desugarPatternBwd σ πs
      (πs × _) <$> desugarBwd e s

{- σ, cs ↘ c -}
instance branches :: DesugarBwd (Elim Boolean) (NonEmptyList (NonEmptyList Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) =
      NonEmptyList <$> (desugarBwd σ b1 `lift2 (:|)` (toList <$> desugarBwd σ (NonEmptyList (b2 :| bs))))
   desugarBwd σ (NonEmptyList (b :| Nil)) =
      NonEmptyList <$> (desugarBwd σ b `lift2 (:|)` pure Nil)

instance branchesUncurried :: DesugarBwd (Elim Boolean) (NonEmptyList (Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) =
      NonEmptyList <$> (desugarBwd σ b1 `lift2 (:|)` (toList <$> desugarBwd σ (NonEmptyList (b2 :| bs))))
   desugarBwd σ (NonEmptyList (b :| Nil)) =
      NonEmptyList <$> (desugarBwd σ b `lift2 (:|)` pure Nil)

{- untotalise κ πs ↗ κ' -}
untotalise :: Cont 𝔹 -> List (Pattern + ListPatternRest) -> Cont 𝔹
untotalise κ Nil = κ
untotalise (Body _) (_ : _) = error absurd
untotalise None (_ : _) = error "todo" -- is None case essentially Hole?
untotalise (Arg (ElimVar x κ)) (π : πs) =
   case π of
      Left (PVar x') ->
         assert (x == x') $ Arg (ElimVar x (untotalise κ πs))
      Left _ -> error absurd
      Right _ -> error absurd
untotalise (Arg (ElimConstr m)) (π : πs) =
   case π of
      Left (PVar _) -> error absurd
      Left (PConstr c ps) ->
         Arg (ElimConstr (fromFoldable [c × untotalise (mustLookup c m) (map Left ps <> πs)]))
      Left PListEmpty ->
         Arg (ElimConstr (fromFoldable [cNil × untotalise (mustLookup cNil m) πs]))
      Left (PListNonEmpty p o) ->
         Arg (ElimConstr (fromFoldable [cCons × untotalise (mustLookup cCons m) (Left p : Right o : πs)]))
      Right PEnd ->
         Arg (ElimConstr (fromFoldable [cNil × untotalise (mustLookup cNil m) πs]))
      Right (PNext p o) ->
         Arg (ElimConstr (fromFoldable [cCons × untotalise (mustLookup cCons m) (Left p : Right o : πs)]))
