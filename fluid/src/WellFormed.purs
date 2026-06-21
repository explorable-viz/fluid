module WellFormed where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Foldable (foldM, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.List (List, length, nub)
import Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DefiniteAssignment (ClassCtx, Ctx, TyResult(..), fields, mergeRes, overrideCtx, overrideRes, unionDisjoint)
import Util.Map (constMap)
import Effect.Exception (Error)
import Expr (bv, fv)
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), LambdaClause(..), ListRest(..), Module(..), ParagraphElem(..), Pattern(..), Stmt(..), VarDef(..)) as S
import Util (type (×), throw, (×))
import Util.Set ((\\), (∪))

checkProgram :: forall m. MonadError Error m => ClassCtx -> Set Var -> Raw S.Stmt -> m (S.Stmt (TyResult Ctx))
checkProgram λ_external γ0 s = do
   λ_program <- classes s
   λ <- unionDisjoint λ_external λ_program
   snd <$> wellFormed λ (constMap true γ0) s

-- Λ over a module body (each top-level statement contributes).
classesOfModule :: forall m a. MonadError Error m => S.Module a -> m ClassCtx
classesOfModule (S.Module ss) = foldM unionDisjoint Map.empty =<< traverse classes ss

-- TODO: actual module-level WF requires a cross-module Γ (primitives + Λ from
-- builtins). Until that's wired up, accept modules unchecked and annotate trivially.
checkModule :: forall m. MonadError Error m => Raw S.Module -> m Unit
checkModule _ = pure unit

-- Spec metafunction classes(s). Lifted to sequences via Seq; descends into compound
-- statements so any dataclass anywhere in the program participates. Duplicate class
-- names are rejected.
classes :: forall m a. MonadError Error m => S.Stmt a -> m ClassCtx
classes (S.Dataclass c b xs) = pure (Map.singleton c (b × xs))
classes (S.Seq s1 s2) = do
   λ1 <- classes s1
   λ2 <- classes s2
   unionDisjoint λ1 λ2
classes (S.If es elseBranch) = do
   λs <- traverse (classes <<< snd) (NEL.toList es)
   λElse <- maybe (pure Map.empty) classes elseBranch
   foldM unionDisjoint λElse λs
classes (S.Match _ ps) = do
   λs <- traverse (classes <<< snd) (NEL.toList ps)
   foldM unionDisjoint Map.empty λs
classes _ = pure Map.empty

assigns :: forall a. S.Stmt a -> Set Var
assigns S.Pass = Set.empty
assigns (S.Def (S.VarDef p _)) = bv p
assigns (S.ExprStmt _) = Set.empty
assigns (S.Assert _ _) = Set.empty
assigns (S.Return _) = Set.empty
assigns (S.If es s) = unions (assigns <$> (snd <$> es)) ∪ maybe Set.empty assigns s
assigns (S.Match _ ps) = unions (assigns <$> (snd <$> ps))
assigns (S.DefRec ds) = unions (Set.singleton <<< fst <$> ds)
assigns (S.Seq s1 s2) = assigns s1 ∪ assigns s2
assigns (S.Dataclass _ _ _) = Set.empty

captures :: forall a. S.Stmt a -> Set Var
captures S.Pass = Set.empty
captures (S.Def (S.VarDef _ e)) = capturesE e
captures (S.ExprStmt e) = capturesE e
captures (S.Assert e e') = capturesE e ∪ maybe Set.empty capturesE e'
captures (S.Return e) = capturesE e
captures (S.If es s) =
   unions ((\(e × s') -> capturesE e ∪ captures s') <$> es) ∪ maybe Set.empty captures s
captures (S.Match e ps) =
   capturesE e ∪ unions ((\(_ × s) -> captures s) <$> ps)
captures (S.DefRec ds) =
   (unions (clauseCaptures <$> ds)) \\ unions (Set.singleton <<< fst <$> ds)
   where
   clauseCaptures (_ × S.Clause _ (ps × s)) =
      (fv s \\ unions (bv <$> ps)) \\ assigns s
captures (S.Seq s1 s2) = captures s1 ∪ captures s2
captures (S.Dataclass _ _ _) = Set.empty

capturesE :: forall a. S.Expr a -> Set Var
capturesE (S.Var _) = Set.empty
capturesE (S.Op _) = Set.empty
capturesE (S.Int _ _) = Set.empty
capturesE (S.Float _ _) = Set.empty
capturesE (S.Str _ _) = Set.empty
capturesE (S.Constr _ _ es) = unions (capturesE <$> es)
capturesE (S.Dictionary _ es) =
   unions ((\(k × v) -> capturesEntry k ∪ capturesE v) <$> es)
   where
   capturesEntry (S.ExprKey e) = capturesE e
   capturesEntry (S.VarKey _ _) = Set.empty
capturesE (S.Matrix _ e (x × y) e') =
   (capturesE e \\ (Set.singleton x ∪ Set.singleton y)) ∪ capturesE e'
capturesE (S.Lambda (S.LambdaClause (ps × e))) =
   fv e \\ unions (bv <$> ps)
capturesE (S.Project e _) = capturesE e
capturesE (S.DProject e e') = capturesE e ∪ capturesE e'
capturesE (S.App e e') = capturesE e ∪ capturesE e'
capturesE (S.BinaryApp e _ e') = capturesE e ∪ capturesE e'
capturesE (S.UnaryPrefixApp _ e) = capturesE e
capturesE (S.Ternary e e1 e2) = capturesE e ∪ capturesE e1 ∪ capturesE e2
capturesE (S.Paragraph es) = unions (capturesPe <$> es)
   where
   capturesPe (S.Token _) = Set.empty
   capturesPe (S.Unquote e) = capturesE e
capturesE (S.ListEmpty _) = Set.empty
capturesE (S.ListNonEmpty _ e l) = capturesE e ∪ capturesEListRest l
   where
   capturesEListRest (S.End _) = Set.empty
   capturesEListRest (S.Next _ e' l') = capturesE e' ∪ capturesEListRest l'
capturesE (S.ListEnum e1 e2) = capturesE e1 ∪ capturesE e2
capturesE (S.ListComp _ e _) = capturesE e
capturesE (S.DocExpr e e') = capturesE e ∪ capturesE e'

wellFormed :: forall m a. MonadError Error m => ClassCtx -> Ctx -> S.Stmt a -> m (TyResult Ctx × S.Stmt (TyResult Ctx))
wellFormed _ _ S.Pass = pure (Assigns Map.empty × S.Pass)
wellFormed _ γ (S.Return e) = do
   wellFormedExpr γ e
   pure (Returns × S.Return (Assigns Map.empty <$ e))
wellFormed _ γ (S.ExprStmt e) = do
   wellFormedExpr γ e
   pure (Assigns Map.empty × S.ExprStmt (Assigns Map.empty <$ e))
wellFormed _ γ (S.Assert e e') = do
   wellFormedExpr γ e
   for_ e' (wellFormedExpr γ)
   pure (Assigns Map.empty × S.Assert (Assigns Map.empty <$ e) ((Assigns Map.empty <$ _) <$> e'))
wellFormed _ γ (S.Def (S.VarDef p e)) = do
   let xs = bv p
   for_ (Set.toUnfoldable (xs `Set.intersection` capturesE e) :: Array Var) \x ->
      throw $ "Variable captured by its own definition: " <> x
   wellFormedExpr γ e
   pure (Assigns (constMap true xs) × S.Def (S.VarDef p (Assigns Map.empty <$ e)))
wellFormed λ γ (S.DefRec ds) = do
   let fs = unions (Set.singleton <<< fst <$> ds)
   let γ' = γ `overrideCtx` constMap true fs
   ds' <- traverse
      ( \(x × S.Clause _ (ps × s)) -> do
           let xs = unions (bv <$> ps)
           let ys = assigns s \\ xs
           let γ'' = γ' `overrideCtx` constMap true xs `overrideCtx` constMap false ys
           r × s' <- wellFormed λ γ'' s
           pure (x × S.Clause r (ps × s'))
      )
      ds
   pure (Assigns (constMap true fs) × S.DefRec ds')
wellFormed λ γ (S.Seq s1 s2) = do
   r1 × s1' <- wellFormed λ γ s1
   case r1 of
      Returns -> throw "Unreachable statement"
      Assigns δ -> do
         for_ (Set.toUnfoldable (captures s1 `Set.intersection` assigns s2) :: Array Var) \x ->
            throw $ "Captured variable reassigned: " <> x
         r2 × s2' <- wellFormed λ (γ `overrideCtx` δ) s2
         pure (overrideRes r1 r2 × S.Seq s1' s2')
wellFormed λ γ (S.If es elseBranch) = do
   es' <- traverse
      ( \(e × s) -> do
           wellFormedExpr γ e
           r × s' <- wellFormed λ γ s
           pure (r × ((Assigns Map.empty <$ e) × s'))
      )
      es
   rElse × elseBranch' <- case elseBranch of
      Just s -> map Just <$> wellFormed λ γ s
      Nothing -> pure (Assigns Map.empty × Nothing)
   pure (foldl1 mergeRes (NEL.cons rElse (fst <$> es')) × S.If (snd <$> es') elseBranch')
wellFormed λ γ (S.Match e ps) = do
   wellFormedExpr γ e
   ps' <- traverse
      ( \(p × s) -> do
           let xs = bv p
           r × s' <- wellFormed λ (γ `overrideCtx` constMap true xs) s
           pure (overrideRes (Assigns (constMap true xs)) r × (p × s'))
      )
      ps
   pure (foldl1 mergeRes ((fst <$> ps') `NEL.snoc` rFall) × S.Match (Assigns Map.empty <$ e) (snd <$> ps'))
   where
   rFall = case fst (NEL.last ps) of
      S.PVar _ -> Returns
      _ -> Assigns Map.empty
wellFormed λ _ (S.Dataclass c b xs) = do
   when (length (nub xs) /= length xs) $ throw $ "Duplicate field names in class: " <> c
   case b of
      Nothing -> pure unit
      Just base -> do
         inherited <- fields λ base
         let clash = Set.intersection (Set.fromFoldable xs) (Set.fromFoldable inherited)
         when (not Set.isEmpty clash)
            $ throw
            $ "Class " <> c <> " redeclares inherited field(s): "
                 <> show (Set.toUnfoldable clash :: List Var)
   pure (Assigns Map.empty × S.Dataclass c b xs)

wellFormedExpr :: forall m a. MonadError Error m => Ctx -> S.Expr a -> m Unit
wellFormedExpr γ e =
   for_ (fv e) \x -> case Map.lookup x γ of
      Just true -> pure unit
      Just false -> throw $ "Not definitely assigned: " <> x
      Nothing -> throw $ "Unbound name: " <> x

