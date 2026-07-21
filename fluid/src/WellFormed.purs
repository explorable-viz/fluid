module WellFormed where

import Prelude

import Bind (Name, Var, dottedName, prefixOf, properPrefixOf)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, get, mapStateT, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldM, foldr, for_, intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), length, mapMaybe, nub, (:))
import ModuleGraph (ModuleName, builtins, predefinedDeps)
import Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DefiniteAssignment (ClassEntry, VarCxt, Entry(..), Cxt, WfResult(..), classFor, erase, extendCxt, extendCxtWith, fields, mergeRes, overrideRes, unionWith_mergeEq)
import Util.Map (constMap)
import Expr (bv, fv)
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), Import(..), LambdaClause(..), ListRest(..), ListRestPattern(..), Module(..), ParagraphElem(..), Pattern(..), Qualifier(..), Stmt(..), VarDef(..)) as S
import Util (type (×), singleton, whenever, (×), (∩))
import Util.Set ((\\), (∪))

-- Member context and checked statements of a loaded module. The table of
-- loaded modules memoises the load judgement, which the spec re-derives
-- freely (loading is pure, so needs no cache).
type LoadedModule = { cxt :: Cxt, mod :: S.Module (WfResult VarCxt) }

type LoadM = StateT (Map.Map ModuleName LoadedModule) (Either String)

-- Check the program, loading each module on demand as its import is checked.
-- Also return the reduced context (the import layer, erased); the desugared
-- program is a term over it, with module and class entries resolved away.
-- Loading recurses without a guard; it terminates because the module
-- dependency graph is checked acyclic at parse time.
checkProgram
   :: Map.Map ModuleName (Raw S.Module)
   -> Cxt
   -> List S.Import
   -> Raw S.Stmt
   -> Either String { γ :: VarCxt, s :: S.Stmt (WfResult VarCxt), loaded :: Map.Map ModuleName LoadedModule }
checkProgram modules base imports s =
   runStateT program Map.empty <#> \((γ × s') × loaded) -> { γ, s: s', loaded }
   where
   program :: LoadM (VarCxt × S.Stmt (WfResult VarCxt))
   program = do
      layer × γ_imp <- checkImports mainModule imports
      _ × s' <- lift (wellFormed mainModule (Map.insert "__name__" (VarStatus true) γ_imp) s)
      pure (Map.insert "__name__" true (erase layer) × s')

   -- Member context of module q; memoised.
   loadModule :: ModuleName -> LoadM Cxt
   loadModule q = get >>= \loaded -> case Map.lookup q loaded of
      Just { cxt } -> pure cxt
      Nothing -> checking q do
         mod@(S.Module is _) <- maybe (throwError ("Module not parsed: " <> dottedName q)) pure (Map.lookup q modules)
         layer × γ_imp <- checkImports q is
         δ × mod' <- lift (checkStatements q γ_imp mod)
         λ <- lift (classesOfModule q mod)
         let subs = submodules (Map.keys modules) q
         let clash = (Map.keys layer ∪ Map.keys δ ∪ Map.keys λ) ∩ Map.keys subs
         when (not Set.isEmpty clash)
            $ throwError
            $ "Submodule name clash in module " <> dottedName q <> ": " <> intercalate ", " (Set.toUnfoldable clash :: List Var)
         let cxt = (if q == builtins then base else Map.empty) `Map.union` subs `Map.union` (Class <$> λ) `Map.union` (VarStatus <$> δ)
         modify_ (Map.insert q { cxt, mod: mod' })
         pure cxt

   checking :: forall a. ModuleName -> LoadM a -> LoadM a
   checking q = mapStateT (lmap (_ <> "\nChecking module " <> dottedName q))

   -- Import layer × the full in-scope context (implicit predefined base extended by the layer).
   checkImports :: ModuleName -> List S.Import -> LoadM (Cxt × Cxt)
   checkImports enclosing is = do
      seed <- foldM (\acc q -> (acc `Map.union` _) <$> loadModule q) base (predefinedDeps enclosing)
      layer <- foldM (\acc i -> (acc `extendCxtWith` _) <$> importBindings enclosing i) Map.empty is
      pure (layer × (seed `extendCxtWith` layer))

   -- Bindings contributed by one import of the enclosing module.
   importBindings :: ModuleName -> S.Import -> LoadM Cxt
   importBindings enclosing (S.Import q Nothing) = do
      when (enclosing `properPrefixOf` q)
         $ throwError
         $ "Module " <> dottedName enclosing <> " cannot import its own descendant " <> dottedName q
      θ <- ModLoaded q <$> loadModule q
      Map.singleton (NEL.head q) <$> loadsTo Nothing q θ
   importBindings enclosing (S.Import q (Just xs)) = do
      γ <- loadModule q
      _ <- loadsTo (Just enclosing) q (ModLoaded q γ) -- loads q's ancestors; contributes no bindings
      importedMembers q γ xs

   -- Wrap the reference for module q in loaded references for its proper
   -- prefixes, loading each; prefixes of the bound (the enclosing module,
   -- for a from-import) are exempt.
   loadsTo :: Maybe ModuleName -> ModuleName -> Entry -> LoadM Entry
   loadsTo bound q θ = case NEL.fromList init of
      Nothing -> pure θ
      Just q'
         | maybe false (q' `prefixOf` _) bound -> pure θ
         | otherwise -> do
              γ <- loadModule q'
              loadsTo bound q' (ModLoaded q' (γ `extendCxtWith` Map.singleton x θ))
      where
      { init, last: x } = NEL.unsnoc q

   -- Bindings for names imported from module q with member context γ.
   importedMembers :: ModuleName -> Cxt -> List Var -> LoadM Cxt
   importedMembers _ _ Nil = pure Map.empty
   importedMembers q γ (x : xs) = do
      rest <- importedMembers q γ xs
      case Map.lookup x γ of
         Just (Mod q') -> loadModule q' <#> \γ' -> Map.insert x (ModLoaded q' γ') rest
         Just (VarStatus false) -> throwError $ "Not definitely assigned: " <> x
         Just θ -> pure (Map.insert x θ rest)
         Nothing -> throwError $ "Cannot import name " <> x <> " from module " <> dottedName q

-- Stubs for the immediate submodules of q among the program's modules.
submodules :: Set ModuleName -> ModuleName -> Cxt
submodules known q = Map.fromFoldable (mapMaybe sub (Set.toUnfoldable known))
   where
   sub m = let { init, last: x } = NEL.unsnoc m in whenever (NEL.fromList init == Just q) (x × Mod m)

classesOfModule :: forall a. Name -> S.Module a -> Either String (Map.Map Var ClassEntry)
classesOfModule q (S.Module _ ss) =
   case foldr (\s acc -> Just (maybe s (S.Seq s) acc)) Nothing ss of
      Nothing -> pure Map.empty
      Just s -> classes q s

checkStatements :: Name -> Cxt -> Raw S.Module -> Either String (VarCxt × S.Module (WfResult VarCxt))
checkStatements q γ_imp (S.Module imports ss) =
   case foldr (\s acc -> Just (maybe s (S.Seq s) acc)) Nothing ss of
      Nothing -> pure (Map.singleton "__name__" true × S.Module imports Nil)
      Just s -> wellFormed q (Map.insert "__name__" (VarStatus true) γ_imp) s <#> \(r × s') ->
         Map.insert "__name__" true (delta r) × S.Module imports (unSeq s')
   where
   delta (Assigns δ) = δ
   delta Returns = Map.empty
   unSeq (S.Seq s1 s2) = s1 : unSeq s2
   unSeq s = s : Nil

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
capturesE (S.ModMember _ _) = Set.empty
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

wellFormed :: forall a. Name -> Cxt -> S.Stmt a -> Either String (WfResult VarCxt × S.Stmt (WfResult VarCxt))
wellFormed _ _ S.Pass = pure (Assigns Map.empty × S.Pass)
wellFormed _ γ (S.Return e) = do
   e' <- wellFormedExpr γ e
   pure (Returns × S.Return (Assigns Map.empty <$ e'))
wellFormed _ γ (S.ExprStmt e) = do
   e' <- wellFormedExpr γ e
   pure (Assigns Map.empty × S.ExprStmt (Assigns Map.empty <$ e'))
wellFormed _ γ (S.Assert e e') = do
   e1 <- wellFormedExpr γ e
   e2 <- traverse (wellFormedExpr γ) e'
   pure (Assigns Map.empty × S.Assert (Assigns Map.empty <$ e1) ((Assigns Map.empty <$ _) <$> e2))
wellFormed _ γ (S.Def (S.VarDef p e)) = do
   let xs = bv p
   for_ (Set.toUnfoldable (xs `Set.intersection` capturesE e) :: Array Var) \x ->
      throwError $ "Variable captured by its own definition: " <> x
   e' <- wellFormedExpr γ e
   p' <- qualifyPattern γ p
   pure (Assigns (constMap true xs) × S.Def (S.VarDef p' (Assigns Map.empty <$ e')))
wellFormed q γ (S.DefRec ds) = do
   let fs = unions (Set.singleton <<< fst <$> ds)
   let γ' = γ `extendCxt` constMap true fs
   ds' <- traverse
      ( \(x × S.Clause _ (ps × s)) -> do
           let xs = unions (bv <$> ps)
           let ys = assigns s \\ xs
           let γ'' = γ' `extendCxt` constMap true xs `extendCxt` constMap false ys
           ps' <- traverse (qualifyPattern γ') ps
           r × s' <- wellFormed q γ'' s
           pure (x × S.Clause r (ps' × s'))
      )
      ds
   pure (Assigns (constMap true fs) × S.DefRec ds')
wellFormed q γ (S.Seq s1 s2) = do
   r1 × s1' <- wellFormed q γ s1
   case r1 of
      Returns -> throwError "Unreachable statement"
      Assigns δ -> do
         for_ (Set.toUnfoldable (captures s1 `Set.intersection` assigns s2) :: Array Var) \x ->
            throwError $ "Captured variable reassigned: " <> x
         λ1 <- classes q s1
         let γ' = Map.union (Class <$> (λ1 <#> _ { cxt = γ })) (γ `extendCxt` δ)
         r2 × s2' <- wellFormed q γ' s2
         pure (overrideRes r1 r2 × S.Seq s1' s2')
wellFormed q γ (S.If es elseBranch) = do
   es' <- traverse
      ( \(e × s) -> do
           e' <- wellFormedExpr γ e
           r × s' <- wellFormed q γ s
           pure (r × ((Assigns Map.empty <$ e') × s'))
      )
      es
   rElse × elseBranch' <- case elseBranch of
      Just s -> map Just <$> wellFormed q γ s
      Nothing -> pure (Assigns Map.empty × Nothing)
   pure (foldl1 mergeRes (NEL.cons rElse (fst <$> es')) × S.If (snd <$> es') elseBranch')
wellFormed q γ (S.Match e ps) = do
   e' <- wellFormedExpr γ e
   ps' <- traverse
      ( \(p × s) -> do
           let xs = bv p
           p' <- qualifyPattern γ p
           r × s' <- wellFormed q (γ `extendCxt` constMap true xs) s
           pure (overrideRes (Assigns (constMap true xs)) r × (p' × s'))
      )
      ps
   pure (foldl1 mergeRes ((fst <$> ps') `NEL.snoc` rFall) × S.Match (Assigns Map.empty <$ e') (snd <$> ps'))
   where
   rFall = case fst (NEL.last ps) of
      S.PVar _ -> Returns
      _ -> Assigns Map.empty
wellFormed q γ (S.Dataclass c b xs) = do
   when (length (nub xs) /= length xs) $ throwError $ "Duplicate field names in class: " <> c
   case b of
      Nothing -> pure unit
      Just base -> do
         cls <- maybe (throwError $ "Unknown class: " <> base) pure (classFor γ base)
         when (cls.mod /= q) $ throwError $ "Cannot extend imported class: " <> base
         let clash = Set.intersection (Set.fromFoldable xs) (Set.fromFoldable (fields cls))
         when (not Set.isEmpty clash)
            $ throwError
            $ "Class " <> c <> " redeclares inherited field(s): "
                 <> show (Set.toUnfoldable clash :: List Var)
   pure (Assigns Map.empty × S.Dataclass c b xs)

asName :: forall a. S.Expr a -> Maybe Name
asName (S.Var x) = Just (singleton x)
asName (S.Project e y) = asName e <#> (_ <> singleton y)
asName _ = Nothing

resolveName :: Cxt -> Name -> Maybe Entry
resolveName γ name = case NEL.fromList init of
   Nothing -> simpleEntry γ x
   Just q -> case resolveName γ q of
      Just (ModLoaded _ γ') -> simpleEntry γ' x
      _ -> Nothing
   where
   { init, last: x } = NEL.unsnoc name
   simpleEntry g y = case Map.lookup y g of
      Just e@(VarStatus true) -> Just e
      Just e@(ModLoaded _ _) -> Just e
      Just e@(Class _) -> Just e
      _ -> Nothing

-- Validate an expression; rewrite constructor names to fully-qualified form and
-- module projections to ModMember.
wellFormedExpr :: forall a. Cxt -> S.Expr a -> Either String (S.Expr a)
wellFormedExpr = wf
   where
   wf :: Cxt -> S.Expr a -> Either String (S.Expr a)
   wf γ e@(S.Var x) = e <$ var γ x
   wf γ e@(S.Op op) = e <$ var γ op
   wf _ e@(S.Int _ _) = pure e
   wf _ e@(S.Float _ _) = pure e
   wf _ e@(S.Str _ _) = pure e
   wf γ (S.Constr α c es) = case resolveName γ c of
      Just (Class cls) -> do
         let fs = fields cls
         when (length es /= length fs)
            $ throwError
            $ dottedName c <> " expects " <> show (length fs) <> " argument(s); got " <> show (length es)
         S.Constr α (qualified cls c) <$> traverse (wf γ) es
      _ -> throwError $ "Unknown dataclass: " <> dottedName c
   wf γ (S.ConstrKw α c es xes) = case resolveName γ c of
      Just (Class cls) ->
         S.ConstrKw α (qualified cls c) <$> traverse (wf γ) es <*> traverse (\(x × e) -> (x × _) <$> wf γ e) xes
      _ -> throwError $ "Unknown dataclass: " <> dottedName c
   wf γ (S.App e e') = S.App <$> wf γ e <*> wf γ e'
   wf γ (S.BinaryApp e op e') = S.BinaryApp <$> wf γ e <*> (op <$ var γ op) <*> wf γ e'
   wf γ (S.UnaryPrefixApp op e) = var γ op *> (S.UnaryPrefixApp op <$> wf γ e)
   wf γ (S.Ternary c e e') = S.Ternary <$> wf γ c <*> wf γ e <*> wf γ e'
   wf γ (S.Project e y) = case resolveName γ =<< asName e of
      Just (ModLoaded q γ') -> do
         when (not (Map.member y γ'))
            $ throwError
            $ "module " <> dottedName q <> " has no member " <> y
         pure (S.ModMember q y)
      _ -> flip S.Project y <$> wf γ e
   wf _ e@(S.ModMember _ _) = pure e
   wf γ (S.DProject e e') = S.DProject <$> wf γ e <*> wf γ e'
   wf γ (S.Matrix α body (x × y) source) =
      (\source' body' -> S.Matrix α body' (x × y) source') <$> wf γ source <*> wf (assignedIn γ (Set.singleton x ∪ Set.singleton y)) body
   wf γ (S.Lambda (S.LambdaClause (ps × e))) = do
      ps' <- traverse (qualifyPattern γ) ps
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
            p' <- qualifyPattern γ' p
            map (S.ListCompGen p' src' : _) <$> qualifiers (assignedIn γ' (bv p)) qs
         S.ListCompDecl (S.VarDef p src) -> do
            src' <- wf γ' src
            p' <- qualifyPattern γ' p
            map (S.ListCompDecl (S.VarDef p' src') : _) <$> qualifiers (assignedIn γ' (bv p)) qs
   wf γ (S.DocExpr e e') = S.DocExpr <$> wf γ e <*> wf γ e'

   qualified cls c = NEL.snoc cls.mod (NEL.last c)

var :: Cxt -> Var -> Either String Unit
var γ x = case Map.lookup x γ of
   Just (VarStatus true) -> pure unit
   Just (VarStatus false) -> throwError $ "Not definitely assigned: " <> x
   Just (Mod q) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just (ModLoaded q _) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just (Class _) -> throwError $ "class " <> x <> " is not a value"
   Nothing -> throwError $ "Unbound name: " <> x

assignedIn :: Cxt -> Set Var -> Cxt
assignedIn γ xs = γ `extendCxt` constMap true xs

qualifyPattern :: Cxt -> S.Pattern -> Either String S.Pattern
qualifyPattern γ = qualify
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
   fqnOf c = case resolveName γ c of
      Just (Class cls) -> pure (NEL.snoc cls.mod (NEL.last c))
      _ -> throwError $ "Unknown dataclass: " <> dottedName c

