module WellFormed where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Foldable (foldM, foldMap, foldr, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), length, nub, (:))
import ModuleGraph (ModuleName)
import Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DataType (arity)
import DefiniteAssignment (ClassCtx, Ctx, Entry(..), Cxt, TyResult(..), classesOf, extendStatuses, fields, mergeRes, overrideRes, unionWith_mergeEq)
import Util.Map (constMap, findWithDefault)
import Effect.Exception (Error)
import Expr (bv, fv)
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), LambdaClause(..), ListRest(..), Module(..), ParagraphElem(..), Pattern(..), Stmt(..), VarDef(..)) as S
import Util (type (×), throw, (×))
import Util.Set ((\\), (∪))

checkProgram :: forall m. MonadError Error m => Map.Map ModuleName Cxt -> Cxt -> Raw S.Stmt -> m (S.Stmt (TyResult Ctx))
checkProgram memo baseCxt s = checkTopLevelImports s *> (snd <$> wellFormed memo baseCxt s)

-- Imports may appear only on the top-level statement spine, not nested in
-- if/match/def blocks.
checkTopLevelImports :: forall m a. MonadError Error m => S.Stmt a -> m Unit
checkTopLevelImports = spine
   where
   spine (S.Seq s1 s2) = spine s1 *> spine s2
   spine (S.Import _ _) = pure unit
   spine s = for_ (nestedImports s) \q -> throw $ "Import not at top level: " <> q

nestedImports :: forall a. S.Stmt a -> List ModuleName
nestedImports (S.Import q _) = q : Nil
nestedImports (S.Seq s1 s2) = nestedImports s1 <> nestedImports s2
nestedImports (S.If es elseBranch) = foldMap (nestedImports <<< snd) es <> maybe Nil nestedImports elseBranch
nestedImports (S.Match _ ps) = foldMap (nestedImports <<< snd) ps
nestedImports (S.DefRec ds) = foldMap (\(_ × S.Clause _ (_ × s)) -> nestedImports s) ds
nestedImports _ = Nil

classesOfModule :: forall m a. MonadError Error m => String -> S.Module a -> m ClassCtx
classesOfModule q (S.Module ss) = foldM unionWith_mergeEq Map.empty =<< traverse (classes q) ss

checkModule :: forall m. MonadError Error m => Map.Map ModuleName Cxt -> Cxt -> Raw S.Module -> m Ctx
checkModule memo γ (S.Module ss) =
   case foldr (\s acc -> Just (maybe s (S.Seq s) acc)) Nothing ss of
      Nothing -> pure Map.empty
      Just s -> checkTopLevelImports s *> wellFormed memo γ s <#> \(r × _) -> case r of
         Assigns δ -> δ
         Returns -> Map.empty

-- Entry program's module (spec entry point E; its __name__ is "__main__").
mainModule :: String
mainModule = "__main__"

classes :: forall m a. MonadError Error m => String -> S.Stmt a -> m ClassCtx
classes q (S.Dataclass c b xs) = pure (Map.singleton c { mod: q, base: b, fields: xs })
classes q (S.Seq s1 s2) = do
   λ1 <- classes q s1
   λ2 <- classes q s2
   unionWith_mergeEq λ1 λ2
classes _ _ = pure Map.empty

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
assigns (S.Import _ _) = Set.empty

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
captures (S.Import _ _) = Set.empty

capturesE :: forall a. S.Expr a -> Set Var
capturesE (S.Var _) = Set.empty
capturesE (S.Op _) = Set.empty
capturesE (S.Int _ _) = Set.empty
capturesE (S.Float _ _) = Set.empty
capturesE (S.Str _ _) = Set.empty
capturesE (S.Constr _ _ es) = unions (capturesE <$> es)
capturesE (S.ConstrKw _ _ es xes) = unions (capturesE <$> es) ∪ unions ((capturesE <<< snd) <$> xes)
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

importedCxt :: forall a. Map.Map ModuleName Cxt -> S.Stmt a -> Cxt
importedCxt memo (S.Import q Nothing) =
   -- spec `import` rule: additionally bind the module name to ModEntry(q).
   -- Single-segment only for now; dotted packages deferred.
   let
      γ = findWithDefault Map.empty q memo
   in
      if contains (Pattern "/") q then γ else Map.insert q (Module q) γ
importedCxt memo (S.Import q (Just xs)) =
   Map.filterKeys (_ `Set.member` Set.fromFoldable xs) (findWithDefault Map.empty q memo)
importedCxt _ _ = Map.empty

wellFormed :: forall m a. MonadError Error m => Map.Map ModuleName Cxt -> Cxt -> S.Stmt a -> m (TyResult Ctx × S.Stmt (TyResult Ctx))
wellFormed _ _ S.Pass = pure (Assigns Map.empty × S.Pass)
wellFormed memo γ (S.Return e) = do
   wellFormedExpr memo γ e
   pure (Returns × S.Return (Assigns Map.empty <$ e))
wellFormed memo γ (S.ExprStmt e) = do
   wellFormedExpr memo γ e
   pure (Assigns Map.empty × S.ExprStmt (Assigns Map.empty <$ e))
wellFormed memo γ (S.Assert e e') = do
   wellFormedExpr memo γ e
   for_ e' (wellFormedExpr memo γ)
   pure (Assigns Map.empty × S.Assert (Assigns Map.empty <$ e) ((Assigns Map.empty <$ _) <$> e'))
wellFormed memo γ (S.Def (S.VarDef p e)) = do
   let xs = bv p
   for_ (Set.toUnfoldable (xs `Set.intersection` capturesE e) :: Array Var) \x ->
      throw $ "Variable captured by its own definition: " <> x
   wellFormedExpr memo γ e
   pure (Assigns (constMap true xs) × S.Def (S.VarDef p (Assigns Map.empty <$ e)))
wellFormed memo γ (S.DefRec ds) = do
   let fs = unions (Set.singleton <<< fst <$> ds)
   let γ' = γ `extendStatuses` constMap true fs
   ds' <- traverse
      ( \(x × S.Clause _ (ps × s)) -> do
           let xs = unions (bv <$> ps)
           let ys = assigns s \\ xs
           let γ'' = γ' `extendStatuses` constMap true xs `extendStatuses` constMap false ys
           r × s' <- wellFormed memo γ'' s
           pure (x × S.Clause r (ps × s'))
      )
      ds
   pure (Assigns (constMap true fs) × S.DefRec ds')
wellFormed memo γ (S.Seq s1 s2) = do
   r1 × s1' <- wellFormed memo γ s1
   case r1 of
      Returns -> throw "Unreachable statement"
      Assigns δ -> do
         for_ (Set.toUnfoldable (captures s1 `Set.intersection` assigns s2) :: Array Var) \x ->
            throw $ "Captured variable reassigned: " <> x
         λ1 <- classes mainModule s1
         let γ' = Map.union (importedCxt memo s1) (Map.union (Class <$> λ1) (γ `extendStatuses` δ))
         r2 × s2' <- wellFormed memo γ' s2
         pure (overrideRes r1 r2 × S.Seq s1' s2')
wellFormed memo γ (S.If es elseBranch) = do
   es' <- traverse
      ( \(e × s) -> do
           wellFormedExpr memo γ e
           r × s' <- wellFormed memo γ s
           pure (r × ((Assigns Map.empty <$ e) × s'))
      )
      es
   rElse × elseBranch' <- case elseBranch of
      Just s -> map Just <$> wellFormed memo γ s
      Nothing -> pure (Assigns Map.empty × Nothing)
   pure (foldl1 mergeRes (NEL.cons rElse (fst <$> es')) × S.If (snd <$> es') elseBranch')
wellFormed memo γ (S.Match e ps) = do
   wellFormedExpr memo γ e
   ps' <- traverse
      ( \(p × s) -> do
           let xs = bv p
           r × s' <- wellFormed memo (γ `extendStatuses` constMap true xs) s
           pure (overrideRes (Assigns (constMap true xs)) r × (p × s'))
      )
      ps
   pure (foldl1 mergeRes ((fst <$> ps') `NEL.snoc` rFall) × S.Match (Assigns Map.empty <$ e) (snd <$> ps'))
   where
   rFall = case fst (NEL.last ps) of
      S.PVar _ -> Returns
      _ -> Assigns Map.empty
wellFormed _ γ (S.Dataclass c b xs) = do
   when (length (nub xs) /= length xs) $ throw $ "Duplicate field names in class: " <> c
   case b of
      Nothing -> pure unit
      Just base -> do
         inherited <- fields (classesOf γ) base
         let clash = Set.intersection (Set.fromFoldable xs) (Set.fromFoldable inherited)
         when (not Set.isEmpty clash)
            $ throw
            $ "Class " <> c <> " redeclares inherited field(s): "
                 <> show (Set.toUnfoldable clash :: List Var)
   pure (Assigns Map.empty × S.Dataclass c b xs)
wellFormed memo _ (S.Import q f) = do
   let γ = findWithDefault Map.empty q memo
   for_ f \xs -> for_ xs \x ->
      when (not (Map.member x γ)) $ throw $ "Cannot import name " <> x <> " from module " <> q
   pure (Assigns Map.empty × S.Import q f)

-- spec `simple-module`: a name bound to ModEntry(q) resolves to module q.
namesModule :: forall a. Cxt -> S.Expr a -> Maybe ModuleName
namesModule γ (S.Var x) = case Map.lookup x γ of
   Just (Module q) -> Just q
   _ -> Nothing
namesModule _ _ = Nothing

wellFormedExpr :: forall m a. MonadError Error m => Map.Map ModuleName Cxt -> Cxt -> S.Expr a -> m Unit
wellFormedExpr memo γ e = do
   for_ (fv e) \x -> case Map.lookup x γ of
      Just (VarStatus false) -> throw $ "Not definitely assigned: " <> x
      Nothing -> throw $ "Unbound name: " <> x
      _ -> pure unit -- VarStatus true, or a class/module name (unconditionally in scope)
   checkExpr memo γ e

checkExpr :: forall m a. MonadError Error m => Map.Map ModuleName Cxt -> Cxt -> S.Expr a -> m Unit
checkExpr memo γ = go
   where
   λ = classesOf γ

   go :: S.Expr a -> m Unit
   go (S.Constr _ c es) = do
      n <- maybe (throw $ "Unknown constructor: " <> c) pure (arity λ c)
      when (length es /= n)
         $ throw
         $ c <> " expects " <> show n <> " argument(s); got " <> show (length es)
      for_ es go
   go (S.ConstrKw _ _ es xes) = for_ es go *> for_ (xes <#> snd) go
   go (S.App e e') = go e *> go e'
   go (S.BinaryApp e _ e') = go e *> go e'
   go (S.UnaryPrefixApp _ e) = go e
   go (S.Ternary c e e') = go c *> go e *> go e'
   -- spec `attr-module`: member of a module must be in its context; otherwise (attr-object) unchecked.
   go (S.Project e y) = do
      case namesModule γ e of
         Just q -> when (not (Map.member y (findWithDefault Map.empty q memo)))
            $ throw
            $ "module " <> q <> " has no member " <> y
         Nothing -> pure unit
      go e
   go (S.DProject e e') = go e *> go e'
   go (S.Matrix _ e _ e') = go e *> go e'
   go (S.Lambda (S.LambdaClause (_ × e))) = go e
   go (S.Dictionary _ kvs) = for_ kvs \(k × v) -> goKey k *> go v
      where
      goKey (S.ExprKey e) = go e
      goKey (S.VarKey _ _) = pure unit
   go (S.Paragraph elems) = for_ elems \el -> case el of
      S.Unquote e -> go e
      S.Token _ -> pure unit
   go (S.ListEmpty _) = pure unit
   go (S.ListNonEmpty _ e l) = go e *> goRest l
      where
      goRest (S.End _) = pure unit
      goRest (S.Next _ e' l') = go e' *> goRest l'
   go (S.ListEnum e e') = go e *> go e'
   go (S.ListComp _ e _) = go e
   go (S.DocExpr e e') = go e *> go e'
   go _ = pure unit

