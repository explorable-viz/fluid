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
import SExpr (Clause, Expr(..), ListRest(..), Pattern(..), ListPatternRest(..), Qualifier(..), VarDef(..))
import Lattice (𝔹, (∨))
import Util (MayFail, type(+), type (×), (×), (≞), (≜), absurd, assert, mustLookup, error)

qualTrue :: 𝔹 -> Qualifier 𝔹
qualTrue α = Guard (Constr α cTrue Nil)

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
   desugarBwd _ (NonEmptyList (_ :| _) × _) = error absurd

instance recDefs :: DesugarBwd (Bindings Elim Boolean) (NonEmptyList (String × (NonEmptyList Pattern × Expr Boolean))) where
   desugarBwd xσs xcs = join <$> zipRecDefs xσs (reverse (groupBy (eq `on` fst) xcs))

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
   desugarBwd (E.App (E.Lambda σ) e) (MatchAs s bs) =
      MatchAs <$> desugarBwd e s <*> desugarBwd σ bs
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
   desugarBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
      ListEnum <$> desugarBwd e1 s1 <*> desugarBwd e2 s2
   -- list-comp-done
   desugarBwd (E.Constr α2 c (e : (E.Constr α1 c' Nil) : Nil))
              (ListComp _ s_body (NonEmptyList (Guard (Constr _ c'' Nil) :| Nil)))
      | c == cCons , c' == cNil, c'' == cTrue =
      ListComp (α1 ∨ α2) <$> desugarBwd e s_body
                         <*> pure (NonEmptyList (Guard (Constr (α1 ∨ α2) cTrue Nil) :| Nil))
   -- list-comp-last
   desugarBwd e (ListComp α s (NonEmptyList (q :| Nil))) = do
      s'' <- desugarBwd e (ListComp α s (NonEmptyList (q :| qualTrue true : Nil)))
      case s'' of
         ListComp β s' (NonEmptyList (q' :| (Guard (Constr _ c Nil)) : Nil)) | c == cTrue ->
            pure (ListComp β s' (NonEmptyList (q' :| Nil)))
         sListComp' -> error absurd
   -- list-comp-guard
   desugarBwd (E.App (E.Lambda (ElimConstr m)) e2)
              (ListComp α0 s1 (NonEmptyList (Guard s2 :| q : qs))) = do
      s2' <- desugarBwd e2 s2
      sListComp <- desugarBwd (asExpr (mustLookup cTrue m)) (ListComp α0 s1 (NonEmptyList (q :| qs)))
      sNil <- desugarBwd (asExpr (mustLookup cFalse m)) (snil true)
      case sListComp, sNil of
         ListComp β s1' (NonEmptyList (q' :| qs')), Constr α c Nil | c == cNil ->
            pure (ListComp (α ∨ β) s1' (NonEmptyList (Guard s2' :| q' : qs')))
         _, _ -> error absurd
   -- list-comp-decl
   desugarBwd (E.App (E.Lambda σ) e)
              (ListComp α0 s2 (NonEmptyList ((Declaration (VarDef π s1)) :| q : qs))) = do
      (_ × sListComp)  <- desugarBwd σ (NonEmptyList (π :| Nil) × (ListComp α0 s2 (NonEmptyList (q :| qs))))
      s1' <- desugarBwd e s1
      case sListComp of
         ListComp β s2' (NonEmptyList (q' :| qs')) ->
            pure (ListComp β s2' (NonEmptyList ((Declaration (VarDef π s1')) :| q' : qs')))
         _ -> error absurd
   -- list-comp-gen
   desugarBwd (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1)
              (ListComp α s2 (NonEmptyList (Generator p s1 :| q : qs))) = do
      s1' <- desugarBwd e1 s1
      let σ' × β = totalise_bwd (Arg σ) (Left p : Nil)
      e2 <- asExpr <$> desugarPatternBwd (asElim σ') p
      sListComp <- desugarBwd e2 (ListComp α s2 (NonEmptyList (q :| qs)))
      case sListComp of
         ListComp β' s2' (NonEmptyList (q' :| qs')) ->
            pure (ListComp (β ∨ β') s2' (NonEmptyList (Generator p s1 :| q' : qs')))
         _ -> error absurd
   desugarBwd (E.Let d e) (Let ds s) = do
      ds' × s' <- desugarBwd (E.Let d e) (ds × s)
      pure (Let ds' s')
   desugarBwd (E.LetRec fπs e) (LetRec fπs' s) =
      LetRec <$> desugarBwd fπs fπs' <*> desugarBwd e s
   desugarBwd (E.Hole) s = error "todo"
   desugarBwd _ _ = error absurd

instance listRest :: DesugarBwd (E.Expr Boolean) (ListRest Boolean) where
   desugarBwd e l@(End _) = case e of
      E.Constr α c Nil ->
         assert (c == cNil) $
         pure (End α)
      E.Constr _ _ _ -> error absurd
      E.Hole -> desugarBwd (E.Constr false cNil Nil) l
      _ -> error absurd
   desugarBwd e l@(Next _ s l') = case e of
      E.Constr α c (e1 : e2 : Nil) ->
         assert (c == cCons) $
         Next α <$> desugarBwd e1 s <*> desugarBwd e2 l'
      E.Constr _ _ _ -> error absurd
      E.Hole -> desugarBwd (E.Constr false cCons (E.Hole : E.Hole : Nil)) l
      _ -> error absurd

class DesugarPatternBwd a where
   desugarPatternBwd :: Elim Boolean -> a -> MayFail (Cont Boolean)

instance patterns :: DesugarPatternBwd (NonEmptyList Pattern) where
   desugarPatternBwd σ (NonEmptyList (π :| Nil)) = desugarPatternBwd σ π
   desugarPatternBwd σ (NonEmptyList (π :| π' : πs)) = do
      test <- desugarPatternBwd σ π
      σ' <- asElim <$> desugarPatternBwd σ π
      desugarPatternBwd σ' (NonEmptyList (π' :| πs))

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

instance patternRest :: DesugarPatternBwd ListPatternRest where
   desugarPatternBwd (ElimVar _ _) _ = error absurd
   desugarPatternBwd (ElimConstr m) PEnd = pure (mustLookup cCons m)
   desugarPatternBwd (ElimConstr m) (PNext π o) = do
      σ' <- asElim <$> desugarPatternBwd (asElim (mustLookup cCons m)) π
      desugarPatternBwd σ' o

instance branch :: DesugarBwd (Elim Boolean) (NonEmptyList Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e <- asExpr <$> desugarPatternBwd σ πs
      (πs × _) <$> desugarBwd e s

instance branchUncurried :: DesugarBwd (Elim Boolean) (Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e <- asExpr <$> desugarPatternBwd σ πs
      (πs × _) <$> desugarBwd e s

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

totalise_bwd :: Cont 𝔹 -> List (Pattern + ListPatternRest) -> Cont 𝔹 × 𝔹
totalise_bwd κ Nil = κ × false
totalise_bwd (Body _) (_ : _) = error absurd
totalise_bwd None (_ : _) = error "todo" -- is None case essentially Hole?
totalise_bwd (Arg (ElimVar x κ)) (π : πs) =
   case π of
      Left (PVar x') ->
         assert (x == x') $
         let κ × α = totalise_bwd κ πs in
         Arg (ElimVar x κ) × α
      Left _ -> error absurd
      Right _ -> error absurd
totalise_bwd (Arg (ElimConstr m)) (π : πs) =
   case π of
      Left (PVar _) -> error absurd
      Left (PConstr c ps) ->
         let κ × α = totalise_bwd (mustLookup c m) (map Left ps <> πs) in
         Arg (ElimConstr (fromFoldable [c × κ])) × α
      Left PListEmpty ->
         let κ × α = totalise_bwd (mustLookup cNil m) πs in
         Arg (ElimConstr (fromFoldable [cNil × κ])) × α
      Left (PListNonEmpty p o) ->
         let κ × α = totalise_bwd (mustLookup cCons m) (Left p : Right o : πs) in
         Arg (ElimConstr (fromFoldable [cCons × κ])) × α
      Right PEnd ->
         let κ × α = totalise_bwd (mustLookup cNil m) πs in
         Arg (ElimConstr (fromFoldable [cNil × κ])) × α
      Right (PNext p o) ->
         let κ × α = totalise_bwd (mustLookup cCons m) (Left p : Right o : πs) in
         Arg (ElimConstr (fromFoldable [cCons × κ])) × α
