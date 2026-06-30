module WellFormed where

import Prelude

import Bind (Name, Var, dottedName)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either)
import Data.Foldable (foldM, foldMap, foldr, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), length, nub, (:))
import ModuleGraph (ModuleName)
import Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DefiniteAssignment (ClassCtx, Ctx, Entry(..), Cxt, TyResult(..), classesOf, extendStatuses, fields, mergeRes, overrideRes, unionWith_mergeEq)
import Util.Map (constMap, findWithDefault)
import Expr (bv, fv)
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), LambdaClause(..), ListRest(..), Module(..), ParagraphElem(..), Pattern(..), Qualifier(..), Stmt(..), VarDef(..), ctrName) as S
import Util (type (×), singleton, (×))
import Util.Set ((\\), (∪))

checkProgram :: Map.Map ModuleName Cxt -> Cxt -> Raw S.Stmt -> Either String (S.Stmt (TyResult Ctx))
checkProgram memo baseCxt s = checkTopLevelImports s *> (snd <$> wellFormed memo baseCxt s)

checkTopLevelImports :: forall a. S.Stmt a -> Either String Unit
checkTopLevelImports = spine
   where
   spine (S.Seq s1 s2) = spine s1 *> spine s2
   spine (S.Import _ _) = pure unit
   spine s = for_ (nestedImports s) \q -> throwError $ "Import not at top level: " <> dottedName q

nestedImports :: forall a. S.Stmt a -> List ModuleName
nestedImports (S.Import q _) = q : Nil
nestedImports (S.Seq s1 s2) = nestedImports s1 <> nestedImports s2
nestedImports (S.If es elseBranch) = foldMap (nestedImports <<< snd) es <> maybe Nil nestedImports elseBranch
nestedImports (S.Match _ ps) = foldMap (nestedImports <<< snd) ps
nestedImports (S.DefRec ds) = foldMap (\(_ × S.Clause _ (_ × s)) -> nestedImports s) ds
nestedImports _ = Nil

classesOfModule :: forall a. Name -> S.Module a -> Either String ClassCtx
classesOfModule q (S.Module ss) = foldM unionWith_mergeEq Map.empty =<< traverse (classes q) ss

checkModule :: Map.Map ModuleName Cxt -> Cxt -> Raw S.Module -> Either String Ctx
checkModule memo γ (S.Module ss) =
   case foldr (\s acc -> Just (maybe s (S.Seq s) acc)) Nothing ss of
      Nothing -> pure Map.empty
      Just s -> checkTopLevelImports s *> wellFormed memo γ s <#> \(r × _) -> case r of
         Assigns δ -> δ
         Returns -> Map.empty

-- Entry program's module (spec entry point E; its __name__ is "__main__").
mainModule :: Name
mainModule = pure "__main__"

classes :: forall a. Name -> S.Stmt a -> Either String ClassCtx
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
importedCxt memo (S.Import q Nothing) = Map.insert x1 (Module (singleton x1)) γ
   where
   x1 = NEL.head q
   γ = findWithDefault Map.empty q memo
importedCxt memo (S.Import q (Just xs)) =
   Map.filterKeys (_ `Set.member` Set.fromFoldable xs) (findWithDefault Map.empty q memo)
importedCxt _ _ = Map.empty

wellFormed :: forall a. Map.Map ModuleName Cxt -> Cxt -> S.Stmt a -> Either String (TyResult Ctx × S.Stmt (TyResult Ctx))
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
      throwError $ "Variable captured by its own definition: " <> x
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
      Returns -> throwError "Unreachable statement"
      Assigns δ -> do
         for_ (Set.toUnfoldable (captures s1 `Set.intersection` assigns s2) :: Array Var) \x ->
            throwError $ "Captured variable reassigned: " <> x
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
   when (length (nub xs) /= length xs) $ throwError $ "Duplicate field names in class: " <> c
   case b of
      Nothing -> pure unit
      Just base -> do
         inherited <- fields (classesOf γ) base
         let clash = Set.intersection (Set.fromFoldable xs) (Set.fromFoldable inherited)
         when (not Set.isEmpty clash)
            $ throwError
            $ "Class " <> c <> " redeclares inherited field(s): "
                 <> show (Set.toUnfoldable clash :: List Var)
   pure (Assigns Map.empty × S.Dataclass c b xs)
wellFormed memo _ (S.Import q f) = do
   let γ = findWithDefault Map.empty q memo
   for_ f \xs -> for_ xs \x ->
      when (not (Map.member x γ)) $ throwError $ "Cannot import name " <> x <> " from module " <> dottedName q
   pure (Assigns Map.empty × S.Import q f)

asName :: forall a. S.Expr a -> Maybe Name
asName (S.Var x) = Just (singleton x)
asName (S.Project e y) = asName e <#> (_ <> singleton y)
asName _ = Nothing

resolveName :: Map.Map ModuleName Cxt -> Cxt -> Name -> Maybe Entry
resolveName memo γ name = case NEL.fromList init of
   Nothing -> simpleEntry
   Just prefix -> case resolveName memo γ prefix of
      Just (Module q) -> qualifiedEntry q
      _ -> Nothing
   where
   { init, last: x } = NEL.unsnoc name
   simpleEntry = case Map.lookup x γ of
      Just (Module q) -> Just (Module q)
      Just (Class c) -> Just (Class c)
      _ -> Nothing
   qualifiedEntry q = case Map.lookup x (findWithDefault Map.empty q memo) of
      Just (Class c) -> Just (Class c)
      _
         | Map.member (NEL.snoc q x) memo -> Just (Module (NEL.snoc q x))
         | otherwise -> Nothing

wellFormedExpr :: forall a. Map.Map ModuleName Cxt -> Cxt -> S.Expr a -> Either String Unit
wellFormedExpr memo = wf
   where
   wf :: Cxt -> S.Expr a -> Either String Unit
   wf γ (S.Var x) = var γ x
   wf γ (S.Op op) = var γ op
   wf _ (S.Int _ _) = pure unit
   wf _ (S.Float _ _) = pure unit
   wf _ (S.Str _ _) = pure unit
   wf γ (S.Constr _ c es) = case resolveName memo γ c of
      Just (Class cls) -> do
         fs <- fields (Map.union (maybe Map.empty classesOf (Map.lookup cls.mod memo)) (classesOf γ)) (S.ctrName c)
         when (length es /= length fs)
            $ throwError
            $ dottedName c <> " expects " <> show (length fs) <> " argument(s); got " <> show (length es)
         for_ es (wf γ)
      _ -> throwError $ "Unknown constructor: " <> dottedName c
   wf γ (S.ConstrKw _ _ es xes) = for_ es (wf γ) *> for_ (xes <#> snd) (wf γ)
   wf γ (S.App e e') = wf γ e *> wf γ e'
   wf γ (S.BinaryApp e op e') = wf γ e *> var γ op *> wf γ e'
   wf γ (S.UnaryPrefixApp op e) = var γ op *> wf γ e
   wf γ (S.Ternary c e e') = wf γ c *> wf γ e *> wf γ e'
   wf γ (S.Project e y) = case resolveName memo γ =<< asName e of
      Just (Module q) -> when (not (Map.member y (findWithDefault Map.empty q memo)))
         $ throwError
         $ "module " <> dottedName q <> " has no member " <> y
      _ -> wf γ e
   wf γ (S.DProject e e') = wf γ e *> wf γ e'
   wf γ (S.Matrix _ body (x × y) source) =
      wf γ source *> wf (assignedIn γ (Set.singleton x ∪ Set.singleton y)) body
   wf γ (S.Lambda (S.LambdaClause (ps × e))) = wf (assignedIn γ (unions (bv <$> ps))) e
   wf γ (S.Dictionary _ kvs) = for_ kvs \(k × v) -> dictKey k *> wf γ v
      where
      dictKey (S.ExprKey e) = wf γ e
      dictKey (S.VarKey _ _) = pure unit
   wf γ (S.Paragraph elems) = for_ elems case _ of
      S.Unquote e -> wf γ e
      S.Token _ -> pure unit
   wf _ (S.ListEmpty _) = pure unit
   wf γ (S.ListNonEmpty _ e l) = wf γ e *> listRest l
      where
      listRest (S.End _) = pure unit
      listRest (S.Next _ e' l') = wf γ e' *> listRest l'
   wf γ (S.ListEnum e e') = wf γ e *> wf γ e'
   wf γ (S.ListComp _ e quals) = qualifiers γ quals
      where
      qualifiers γ' Nil = wf γ' e
      qualifiers γ' (q : qs) = case q of
         S.ListCompGuard cond -> wf γ' cond *> qualifiers γ' qs
         S.ListCompGen p src -> wf γ' src *> qualifiers (assignedIn γ' (bv p)) qs
         S.ListCompDecl (S.VarDef p src) -> wf γ' src *> qualifiers (assignedIn γ' (bv p)) qs
   wf γ (S.DocExpr e e') = wf γ e *> wf γ e'

var :: Cxt -> Var -> Either String Unit
var γ x = case Map.lookup x γ of
   Just (VarStatus true) -> pure unit
   Just (VarStatus false) -> throwError $ "Not definitely assigned: " <> x
   Just (Module q) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just (Class _) -> throwError $ "class " <> x <> " is not a value"
   Nothing -> throwError $ "Unbound name: " <> x

assignedIn :: Cxt -> Set Var -> Cxt
assignedIn γ xs = γ `extendStatuses` constMap true xs

