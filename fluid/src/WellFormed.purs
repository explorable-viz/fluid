module WellFormed where

import Prelude

import Bind (Name, Var, dottedName)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either)
import Data.Foldable (foldMap, foldr, for_)
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
import DefiniteAssignment (ClassEntry, Ctx, Entry(..), Cxt, TyResult(..), classFor, extendCxt, fields, mergeRes, overrideRes, unionWith_mergeEq)
import Util.Map (constMap, findWithDefault)
import Expr (bv, fv)
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), LambdaClause(..), ListRest(..), ListRestPattern(..), Module(..), ParagraphElem(..), Pattern(..), Qualifier(..), Stmt(..), VarDef(..)) as S
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

classesOfModule :: forall a. Name -> S.Module a -> Either String (Map.Map Var ClassEntry)
classesOfModule q (S.Module ss) =
   case foldr (\s acc -> Just (maybe s (S.Seq s) acc)) Nothing ss of
      Nothing -> pure Map.empty
      Just s -> classes q s

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

classes :: forall a. Name -> S.Stmt a -> Either String (Map.Map Var ClassEntry)
classes q = go Map.empty
   where
   go acc (S.Dataclass c b xs) =
      unionWith_mergeEq acc (Map.singleton c { cxt: Class <$> acc, mod: q, base: b, fields: xs })
   go acc (S.Seq s1 s2) = go acc s1 >>= \acc' -> go acc' s2
   go acc _ = pure acc

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
importedCxt _ (S.Import q Nothing) = Map.singleton x1 (Module (singleton x1))
   where
   x1 = NEL.head q
importedCxt memo (S.Import q (Just xs)) =
   Map.filterKeys (_ `Set.member` Set.fromFoldable xs) (findWithDefault Map.empty q memo)
importedCxt _ _ = Map.empty

wellFormed :: forall a. Map.Map ModuleName Cxt -> Cxt -> S.Stmt a -> Either String (TyResult Ctx × S.Stmt (TyResult Ctx))
wellFormed _ _ S.Pass = pure (Assigns Map.empty × S.Pass)
wellFormed memo γ (S.Return e) = do
   e' <- wellFormedExpr memo γ e
   pure (Returns × S.Return (Assigns Map.empty <$ e'))
wellFormed memo γ (S.ExprStmt e) = do
   e' <- wellFormedExpr memo γ e
   pure (Assigns Map.empty × S.ExprStmt (Assigns Map.empty <$ e'))
wellFormed memo γ (S.Assert e e') = do
   e1 <- wellFormedExpr memo γ e
   e2 <- traverse (wellFormedExpr memo γ) e'
   pure (Assigns Map.empty × S.Assert (Assigns Map.empty <$ e1) ((Assigns Map.empty <$ _) <$> e2))
wellFormed memo γ (S.Def (S.VarDef p e)) = do
   let xs = bv p
   for_ (Set.toUnfoldable (xs `Set.intersection` capturesE e) :: Array Var) \x ->
      throwError $ "Variable captured by its own definition: " <> x
   e' <- wellFormedExpr memo γ e
   p' <- qualifyPattern memo γ p
   pure (Assigns (constMap true xs) × S.Def (S.VarDef p' (Assigns Map.empty <$ e')))
wellFormed memo γ (S.DefRec ds) = do
   let fs = unions (Set.singleton <<< fst <$> ds)
   let γ' = γ `extendCxt` constMap true fs
   ds' <- traverse
      ( \(x × S.Clause _ (ps × s)) -> do
           let xs = unions (bv <$> ps)
           let ys = assigns s \\ xs
           let γ'' = γ' `extendCxt` constMap true xs `extendCxt` constMap false ys
           ps' <- traverse (qualifyPattern memo γ') ps
           r × s' <- wellFormed memo γ'' s
           pure (x × S.Clause r (ps' × s'))
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
         let γ' = Map.union (importedCxt memo s1) (Map.union (Class <$> (λ1 <#> _ { cxt = γ })) (γ `extendCxt` δ))
         r2 × s2' <- wellFormed memo γ' s2
         pure (overrideRes r1 r2 × S.Seq s1' s2')
wellFormed memo γ (S.If es elseBranch) = do
   es' <- traverse
      ( \(e × s) -> do
           e' <- wellFormedExpr memo γ e
           r × s' <- wellFormed memo γ s
           pure (r × ((Assigns Map.empty <$ e') × s'))
      )
      es
   rElse × elseBranch' <- case elseBranch of
      Just s -> map Just <$> wellFormed memo γ s
      Nothing -> pure (Assigns Map.empty × Nothing)
   pure (foldl1 mergeRes (NEL.cons rElse (fst <$> es')) × S.If (snd <$> es') elseBranch')
wellFormed memo γ (S.Match e ps) = do
   e' <- wellFormedExpr memo γ e
   ps' <- traverse
      ( \(p × s) -> do
           let xs = bv p
           p' <- qualifyPattern memo γ p
           r × s' <- wellFormed memo (γ `extendCxt` constMap true xs) s
           pure (overrideRes (Assigns (constMap true xs)) r × (p' × s'))
      )
      ps
   pure (foldl1 mergeRes ((fst <$> ps') `NEL.snoc` rFall) × S.Match (Assigns Map.empty <$ e') (snd <$> ps'))
   where
   rFall = case fst (NEL.last ps) of
      S.PVar _ -> Returns
      _ -> Assigns Map.empty
wellFormed _ γ (S.Dataclass c b xs) = do
   when (length (nub xs) /= length xs) $ throwError $ "Duplicate field names in class: " <> c
   case b of
      Nothing -> pure unit
      Just base -> do
         inherited <- maybe (throwError $ "Unknown class: " <> base) (pure <<< fields) (classFor γ base)
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

-- Validate an expression and rewrite each constructor name to its fully-qualified
-- form (defining module followed by class name).
wellFormedExpr :: forall a. Map.Map ModuleName Cxt -> Cxt -> S.Expr a -> Either String (S.Expr a)
wellFormedExpr memo = wf
   where
   wf :: Cxt -> S.Expr a -> Either String (S.Expr a)
   wf γ e@(S.Var x) = e <$ var γ x
   wf γ e@(S.Op op) = e <$ var γ op
   wf _ e@(S.Int _ _) = pure e
   wf _ e@(S.Float _ _) = pure e
   wf _ e@(S.Str _ _) = pure e
   wf γ (S.Constr α c es) = case resolveName memo γ c of
      Just (Class ce) -> do
         let fs = fields ce
         when (length es /= length fs)
            $ throwError
            $ dottedName c <> " expects " <> show (length fs) <> " argument(s); got " <> show (length es)
         S.Constr α (qualified ce c) <$> traverse (wf γ) es
      _ -> throwError $ "Unknown dataclass: " <> dottedName c
   wf γ (S.ConstrKw α c es xes) = case resolveName memo γ c of
      Just (Class ce) ->
         S.ConstrKw α (qualified ce c) <$> traverse (wf γ) es <*> traverse (\(x × e) -> (x × _) <$> wf γ e) xes
      _ -> throwError $ "Unknown dataclass: " <> dottedName c
   wf γ (S.App e e') = S.App <$> wf γ e <*> wf γ e'
   wf γ (S.BinaryApp e op e') = S.BinaryApp <$> wf γ e <*> (op <$ var γ op) <*> wf γ e'
   wf γ (S.UnaryPrefixApp op e) = var γ op *> (S.UnaryPrefixApp op <$> wf γ e)
   wf γ (S.Ternary c e e') = S.Ternary <$> wf γ c <*> wf γ e <*> wf γ e'
   wf γ (S.Project e y) = case resolveName memo γ =<< asName e of
      Just (Module q) -> do
         when (not (Map.member y (findWithDefault Map.empty q memo)))
            $ throwError
            $ "module " <> dottedName q <> " has no member " <> y
         pure (S.Project e y)
      _ -> flip S.Project y <$> wf γ e
   wf γ (S.DProject e e') = S.DProject <$> wf γ e <*> wf γ e'
   wf γ (S.Matrix α body (x × y) source) =
      (\source' body' -> S.Matrix α body' (x × y) source') <$> wf γ source <*> wf (assignedIn γ (Set.singleton x ∪ Set.singleton y)) body
   wf γ (S.Lambda (S.LambdaClause (ps × e))) = do
      ps' <- traverse (qualifyPattern memo γ) ps
      e' <- wf (assignedIn γ (unions (bv <$> ps))) e
      pure (S.Lambda (S.LambdaClause (ps' × e')))
   wf γ (S.Dictionary α kvs) = S.Dictionary α <$> traverse (\(k × v) -> (×) <$> dictKey k <*> wf γ v) kvs
      where
      dictKey (S.ExprKey e) = S.ExprKey <$> wf γ e
      dictKey k@(S.VarKey _ _) = pure k
   wf γ (S.Paragraph elems) = S.Paragraph <$> traverse pe elems
      where
      pe (S.Unquote e) = S.Unquote <$> wf γ e
      pe t@(S.Token _) = pure t
   wf _ e@(S.ListEmpty _) = pure e
   wf γ (S.ListNonEmpty α e l) = S.ListNonEmpty α <$> wf γ e <*> listRest l
      where
      listRest l'@(S.End _) = pure l'
      listRest (S.Next α' e' l') = S.Next α' <$> wf γ e' <*> listRest l'
   wf γ (S.ListEnum e e') = S.ListEnum <$> wf γ e <*> wf γ e'
   wf γ (S.ListComp α e quals) = (\(e' × quals') -> S.ListComp α e' quals') <$> qualifiers γ quals
      where
      qualifiers γ' Nil = (_ × Nil) <$> wf γ' e
      qualifiers γ' (q : qs) = case q of
         S.ListCompGuard cond -> do
            cond' <- wf γ' cond
            map (S.ListCompGuard cond' : _) <$> qualifiers γ' qs
         S.ListCompGen p src -> do
            src' <- wf γ' src
            p' <- qualifyPattern memo γ' p
            map (S.ListCompGen p' src' : _) <$> qualifiers (assignedIn γ' (bv p)) qs
         S.ListCompDecl (S.VarDef p src) -> do
            src' <- wf γ' src
            p' <- qualifyPattern memo γ' p
            map (S.ListCompDecl (S.VarDef p' src') : _) <$> qualifiers (assignedIn γ' (bv p)) qs
   wf γ (S.DocExpr e e') = S.DocExpr <$> wf γ e <*> wf γ e'

   qualified ce c = NEL.snoc ce.mod (NEL.last c)

var :: Cxt -> Var -> Either String Unit
var γ x = case Map.lookup x γ of
   Just (VarStatus true) -> pure unit
   Just (VarStatus false) -> throwError $ "Not definitely assigned: " <> x
   Just (Module q) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just (Class _) -> throwError $ "class " <> x <> " is not a value"
   Nothing -> throwError $ "Unbound name: " <> x

assignedIn :: Cxt -> Set Var -> Cxt
assignedIn γ xs = γ `extendCxt` constMap true xs

qualifyPattern :: Map.Map ModuleName Cxt -> Cxt -> S.Pattern -> Either String S.Pattern
qualifyPattern memo γ = qualify
   where
   qualify (S.PConstr c ps) = do
      fqn <- fqnOf c
      S.PConstr fqn <$> traverse qualify ps
   qualify (S.PConstrKw c ps xps) = do
      fqn <- fqnOf c
      S.PConstrKw fqn <$> traverse qualify ps <*> traverse (traverse qualify) xps
   qualify (S.PRecord xps) = S.PRecord <$> traverse (traverse qualify) xps
   qualify (S.PListNonEmpty p lr) = S.PListNonEmpty <$> qualify p <*> qualifyRest lr
   qualify p = pure p
   qualifyRest (S.PListNext p lr) = S.PListNext <$> qualify p <*> qualifyRest lr
   qualifyRest lr = pure lr
   fqnOf c = case resolveName memo γ c of
      Just (Class ce) -> pure (NEL.snoc ce.mod (NEL.last c))
      _ -> throwError $ "Unknown dataclass: " <> dottedName c

