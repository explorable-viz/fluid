module WellFormed where

import Prelude hiding (absurd)

import Bind (Bind, Name, Var, dottedName, prefixOf, properPrefixOf, varThis, (↦))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.State (StateT, gets, mapStateT, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either, hush)
import Control.MonadPlus (guard)
import Data.Foldable (all, and, elem, find, foldM, foldl, for_)
import Data.Function (on)
import Data.FoldableWithIndex (forWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.TraversableWithIndex (forWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.List (List(..), drop, intersect, length, mapMaybe, null, sort, transpose, (:))
import Data.Foldable (lookup) as F
import ModuleGraph (ModuleName, implicitFor, submodules)
import Data.List.NonEmpty (NonEmptyList(..), head, last, snoc, unsnoc)
import Data.NonEmpty ((:|))
import Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst, snd)
import DataType (ClassTable, cParagraph, cRange, ancestors, fieldMap, fieldNames, fields)
import DefiniteAssignment (Entry(..), Cxt, WfResult(..), classOf, extend, mergeRes, override, overrideRes, resolveName)
import Dict as D
import Util.Map (constMap, get, insert, keys, lookup, maplet)
import Expr (bv, fv)
import Expr (Pattern(..)) as S
import Expr (Branch(..), Def(..), Expr(..), Import(..), Module(..), Qualifier(..), RecDefs(..), Stmt(..)) as E
import Literal (Literal(..))
import Pretty (prettyP)
import SExpr (Clause(..), Expr(..), Import(..), LambdaClause(..), Module(..), Param(..), ParagraphElem(..), Qualifier(..), Stmt(..), TypeExpr(..), VarDef(..), assigns, binds, declares) as S
import Types as T
import Util (MayFail, type (×), absurd, checkDistinct, definitely', error, nonEmpty, singleton, tail, zip, zipWith, (×), (∩))
import Util.Pair (Pair(..))
import Util.Set (empty, (\\), (∈), (∪))

-- Predefined modules and program (under __main__) have no body
type CheckedModule = { cxt :: Cxt, mod :: Maybe E.Module }

-- Modules checked so far, over parsed modules.
type CheckState = { modules :: Map.Map ModuleName CheckedModule, classes :: ClassTable }
type CheckM = StateT CheckState (ReaderT (Map.Map ModuleName S.Module) (Either String))

runCheckM :: forall a. CheckM a -> Map.Map ModuleName S.Module -> Map.Map ModuleName Cxt -> ClassTable -> MayFail (a × CheckState)
runCheckM m mods predefined classes =
   runReaderT (runStateT m { modules: predefined <#> \cxt -> { cxt, mod: Nothing }, classes }) mods

checkProgram :: List S.Import -> S.Stmt -> CheckM E.Stmt
checkProgram imports s = do
   cxt_imp × cxt_ιs <- checkImports mainModule imports
   snd <$> (moduleScope cxt_imp cxt_ιs s >>= \cxt_s -> wellFormedTop mainModule cxt_s s)

-- Member context of module q, checked on first import; no cycle guard, as the dependency graph is acyclic.
checkModule :: ModuleName -> CheckM Cxt
checkModule q = gets _.modules >>= \modules -> case lookup q modules of
   Just { cxt } -> pure cxt
   Nothing -> mapStateT (mapReaderT (lmap (_ <> "\nChecking module " <> dottedName q))) do
      mods <- lift ask
      mod@(S.Module ιs _) <- maybe (throwError ("Module not parsed: " <> dottedName q)) pure (lookup q mods)
      cxt_imp × cxt_ιs <- checkImports q ιs
      cxt' × mod' <- checkStatements q cxt_imp cxt_ιs mod
      let subMods = Mod <$> Map.fromFoldable (submodules (keys mods) q)
      for_ (Set.toUnfoldable ((keys cxt_ιs ∪ keys cxt') ∩ keys subMods) :: List Var) \x ->
         throwError $ "Duplicate module member: " <> dottedName q <> " defines " <> x
            <> " and also has submodule "
            <> dottedName (snoc q x)
      let cxt = subMods ∪ cxt'
      modify_ \st -> st { modules = insert q { cxt, mod: Just mod' } st.modules }
      pure cxt

checkImports :: ModuleName -> List S.Import -> CheckM (Cxt × Cxt)
checkImports enclosing ιs = do
   cxt <- foldM (\acc q -> (acc ∪ _) <$> checkModule q) empty (implicitFor enclosing)
   cxt' <- foldM (\acc ι -> (acc `extend` _) <$> importBindings enclosing ι) empty ιs
   pure (cxt × cxt')

-- Bindings contributed by one import of the enclosing module.
importBindings :: ModuleName -> S.Import -> CheckM Cxt
importBindings enclosing (S.Import q Nothing) = do
   when (enclosing `properPrefixOf` q)
      $ throwError
      $ "Module " <> dottedName enclosing <> " cannot import its own descendant " <> dottedName q
   θ <- ModChecked q <$> checkModule q
   maplet (head q) <$> checksTo Nothing q θ
importBindings enclosing (S.Import q (Just xs)) = do
   cxt <- checkModule q
   _ <- checksTo (Just enclosing) q (ModChecked q cxt) -- checks q's ancestors; contributes no bindings
   importedMembers q cxt xs

-- Wrap the reference for module q in checked references for its proper
-- prefixes, checking each; prefixes of the bound (the enclosing module,
-- for a from-import) are exempt.
checksTo :: Maybe ModuleName -> ModuleName -> Entry -> CheckM Entry
checksTo bound q θ = case NEL.fromList init of
   Nothing -> pure θ
   Just q'
      | maybe false (q' `prefixOf` _) bound -> pure θ
      | otherwise -> do
           cxt <- checkModule q'
           checksTo bound q' (ModChecked q' (cxt `extend` maplet x θ))
   where
   { init, last: x } = unsnoc q

-- Bindings for names imported from module q with member context cxt.
importedMembers :: ModuleName -> Cxt -> List Var -> CheckM Cxt
importedMembers _ _ Nil = pure empty
importedMembers q cxt (x : xs) = do
   othersCxt <- importedMembers q cxt xs
   case lookup x cxt of
      Just (Mod q') -> checkModule q' <#> \cxt' -> insert x (ModChecked q' cxt') othersCxt
      Just Declared -> throwError $ "Not definitely initialised: " <> x
      Just PossiblyUnassigned -> throwError $ "Not definitely initialised: " <> x
      Just θ -> pure (insert x θ othersCxt)
      Nothing -> throwError $ "Cannot import name " <> x <> " from module " <> dottedName q

checkStatements :: Name -> Cxt -> Cxt -> S.Module -> CheckM (Cxt × E.Module)
checkStatements q cxt_imp cxt_ιs (S.Module imports s) = do
   r × s' <- moduleScope cxt_imp cxt_ιs s >>= \cxt_s -> wellFormedTop q cxt_s s
   case r of
      -- Program may return, with the returned value as its result; spec treats __main__ as a module
      Returns -> throwError "Module body cannot return"
      Assigns cxt' -> pure (insert "__name__" Assigned cxt' × E.Module imports' s')
   where
   imports' = imports <#> \(S.Import q' xs) -> E.Import q' xs

moduleScope :: Cxt -> Cxt -> S.Stmt -> CheckM Cxt
moduleScope cxt_imp cxt_ιs s = insert "__name__" Assigned <<< (cxt_imp `extend` cxt_ιs `override` _) <$> scope "imported name" (keys cxt_ιs) s

scope :: String -> Set Var -> S.Stmt -> CheckM Cxt
scope what ys s = do
   checkDistinct ("Declared twice: " <> _) (S.declares s)
   for_ (declared ∩ ys) \x -> throwError $ "Redeclared " <> what <> ": " <> x
   for_ (S.binds s ∩ declared) \x -> throwError $ "Pattern variable also declared: " <> x
   for_ (S.binds s ∩ ys) \x -> throwError $ "Pattern variable shadows " <> what <> ": " <> x
   pure (constMap Unbound (S.assigns s \\ ys))
   where
   declared = Set.fromFoldable (S.declares s)

mainModule :: Name
mainModule = pure "__main__"

-- Variables of the enclosing scope captured by lambdas.
class Captures a where
   captures :: a -> Set Var

instance Captures S.Expr where
   captures (S.Var _) = empty
   captures (S.Lit _) = empty
   captures (S.Call e _ es xes) = captures e ∪ unions (captures <$> es) ∪ unions ((captures <<< snd) <$> xes)
   captures (S.Dictionary es) =
      unions ((\(k × v) -> captures k ∪ captures v) <$> es)
   captures (S.Matrix e (x × y) e') =
      (captures e \\ (singleton x ∪ singleton y)) ∪ captures e'
   captures (S.Lambda (S.LambdaClause (ps × e))) =
      fv e \\ unions (bv <$> ps)
   captures (S.Attribute e _) = captures e
   captures (S.Subscript e e') = captures e ∪ captures e'
   captures (S.BinOp e _ e') = captures e ∪ captures e'
   captures (S.UnOp _ e) = captures e
   captures (S.And e e') = captures e ∪ captures e'
   captures (S.Or e e') = captures e ∪ captures e'
   captures (S.InfixApp e _ e') = captures e ∪ captures e'
   captures (S.Cond e1 e e2) = captures e1 ∪ captures e ∪ captures e2
   captures (S.Paragraph es) = unions (capturesPe <$> es)
      where
      capturesPe (S.Token _) = empty
      capturesPe (S.Unquote e) = captures e
   captures (S.List es) = unions (captures <$> es)
   captures (S.Tuple es) = unions (captures <$> es)
   captures (S.ListComp e gs) = captures gs ∪ (captures e \\ bv gs)
   captures (S.DictComp k e gs) = captures gs ∪ ((captures k ∪ captures e) \\ bv gs)
   captures (S.DocExpr e e') = (captures e \\ singleton varThis) ∪ captures e'

instance Captures (List S.Qualifier) where
   captures Nil = empty
   captures (S.Guard e : gs) = captures e ∪ captures gs
   captures (S.Generator p e : gs) = captures e ∪ (captures gs \\ bv p)
   captures (S.Decl (S.VarDef p _ e) : gs) = captures e ∪ (captures gs \\ bv p)

-- Translation into core, with the variables assigned or a return.
class WellFormed a b | a -> b where
   wellFormed :: Cxt -> a -> CheckM b

instance WellFormed S.Stmt (WfResult Cxt × E.Stmt) where
   wellFormed _ S.Pass = pure (Assigns empty × E.Pass)
   wellFormed cxt (S.Return e) = (Returns × _) <<< E.Return <$> wellFormed cxt e
   wellFormed cxt (S.ExprStmt e) = (Assigns empty × _) <<< E.ExprStmt <$> wellFormed cxt e
   wellFormed cxt (S.Assert e e') =
      (Assigns empty × _) <$> (E.Assert <$> wellFormed cxt e <*> traverse (wellFormed cxt) e')
   wellFormed cxt (S.Declare x ψ) = do
      void $ resolveType cxt ψ
      pure (Assigns (maplet x Declared) × E.Declare x)
   wellFormed cxt (S.Def (S.VarDef p ψ e)) = do
      let xs = bv p
      for_ xs (assignable cxt)
      e' <- wellFormed cxt e
      p' <- wellFormed cxt p
      for_ ψ (resolveType cxt)
      pure (Assigns (constMap Assigned xs) × E.Assign p' e')
   wellFormed cxt (S.DefRec ds) = do
      let cxt_fs = constMap Assigned (Set.fromFoldable (fst <$> ds))
      let cxt' = cxt `override` cxt_fs
      let defs = NEL.groupBy (eq `on` fst) ds
      checkDistinct ("Non-contiguous clauses for: " <> _) (NEL.toList (fst <<< head <$> defs))
      defs' <- for defs \def -> do
         void $ wellFormedPatterns cxt' (def <#> \(_ × S.Clause (_ × ps × _)) -> S.PList (ps <#> \(S.Param p _) -> p))
         (fst (head def) ↦ _) <$> (traverse (wellFormedClause cxt' <<< snd) def >>= clauses)
      pure (Assigns cxt_fs × E.DefRec (E.RecDefs (D.fromFoldable defs')))
   wellFormed cxt (S.Seq s1 s2) = do
      r1 × s1' <- wellFormed cxt s1
      case r1 of
         Returns -> throwError "Unreachable statement"
         Assigns cxt' -> do
            r2 × s2' <- wellFormed (cxt `override` cxt') s2
            pure (overrideRes r1 r2 × E.Seq s1' s2')
   wellFormed cxt (S.If es elseBranch) = do
      es' <- for es \(e × s) -> do
         e' <- wellFormed cxt e
         r × s' <- wellFormed cxt s
         pure (r × E.Branch e' s')
      rElse × elseBranch' <- case elseBranch of
         Just s -> map Just <$> wellFormed cxt s
         Nothing -> pure (Assigns empty × Nothing)
      pure (foldl1 mergeRes (NEL.cons rElse (fst <$> es')) × E.If (snd <$> es') elseBranch')
   wellFormed cxt (S.Match e bs) = do
      e' <- wellFormed cxt e
      ps' <- wellFormedPatterns cxt (fst <$> bs)
      bs' <- for (zip ps' bs) \(p' × (p × s)) -> do
         let cxt_xs = constMap Assigned (bv p)
         r × s' <- wellFormed (cxt `override` cxt_xs) s
         pure (overrideRes (Assigns cxt_xs) r × (p' × s'))
      pure (foldl1 mergeRes ((fst <$> bs') `snoc` rNoMatch) × E.Match e' (snd <$> bs'))
      where
      rNoMatch = case fst (last bs) of
         S.PVar _ -> Returns
         S.PWild -> Returns
         _ -> Assigns empty
   wellFormed _ (S.Dataclass _ _ _ _) = error absurd
   wellFormed _ (S.TypeAlias _ _ _) = error absurd

wellFormedTop :: Name -> Cxt -> S.Stmt -> CheckM (WfResult Cxt × E.Stmt)
wellFormedTop q cxt (S.Dataclass c αs b xψs) = do
   predefName cxt "dataclass"
   let cxt' = cxt `override` typeParams αs
   let xs = fst <$> xψs
   checkDistinct (\x -> "Duplicate field " <> x <> " in class " <> c) xs
   xτs <- for xψs \(x × ψ) -> (x × _) <$> resolveType cxt' ψ
   base <- for b \ψ -> case ψ of
      S.NameTy q' ψs | Just (Type d) <- resolveName cxt q' -> do
         σs <- typeArguments cxt' d ψs
         inherited <- gets \{ classes } -> fieldNames classes d
         for_ (xs `intersect` inherited) \x -> throwError $ "Inherited field " <> x <> " redeclared in class " <> c
         pure (d × σs)
      _ -> throwError $ "Base class of " <> c <> " is not a class: " <> prettyP ψ
   let name = snoc q c
   modify_ \st -> st { classes = insert name { typeParams: αs, fields: xτs, base } st.classes }
   pure (Assigns (maplet c (Type name)) × E.Dataclass name)
wellFormedTop _ cxt (S.TypeAlias x αs ψ) = do
   τ <- resolveType (cxt `override` typeParams αs) ψ
   pure (Assigns (maplet x (TypeAlias αs τ)) × E.TypeAlias x αs τ)
wellFormedTop q cxt (S.Seq t1 t2) = do
   r1 × t1' <- wellFormedTop q cxt t1
   case r1 of
      Returns -> throwError "Unreachable statement"
      Assigns cxt' -> do
         r2 × t2' <- wellFormedTop q (cxt `override` cxt') t2
         pure (overrideRes r1 r2 × E.Seq t1' t2')
wellFormedTop _ cxt s = wellFormed cxt s

typeParams :: List Var -> Cxt
typeParams αs = constMap TypeVar (Set.fromFoldable αs)

resolveType :: Cxt -> S.TypeExpr -> CheckM T.Type
resolveType cxt (S.PrimitiveTy ν) = T.PrimitiveTy ν <$ predefName cxt (T.primitiveName ν)
resolveType cxt (S.NameTy q ψs) = case resolveName cxt q of
   Just TypeVar | null ψs -> pure (T.VarTy (dottedName q))
   Just (TypeAlias αs τ) -> do
      τs <- traverse (resolveType cxt) ψs
      when (length τs /= length αs) $ throwError $ arityMismatch "Type alias" q αs τs
      pure (T.subst τs αs τ)
   Just (Type c) -> T.ClassTy c <$> typeArguments cxt c ψs
   Nothing -> throwError $ "Unbound name: " <> dottedName q
   _ -> throwError $ "Not a class: " <> dottedName q
resolveType cxt (S.LitTy ℓ) = T.LitTy ℓ <$ predefName cxt "Literal"
resolveType cxt (S.ListTy ψ) = predefName cxt "list" *> (T.ListTy <$> resolveType cxt ψ)
resolveType cxt (S.DictTy ψ) = predefName cxt "dict" *> predefName cxt "str" *> (T.DictTy <$> resolveType cxt ψ)
resolveType cxt (S.TupleTy ψs) = predefName cxt "tuple" *> (T.TupleTy <$> traverse (resolveType cxt) ψs)
resolveType cxt (S.CallableTy ψs ψ) =
   predefName cxt "Callable" *> (T.CallableTy <$> traverse (resolveType cxt) ψs <*> resolveType cxt ψ)
resolveType cxt (S.TypeTy ψ) = predefName cxt "type" *> resolveType cxt ψ >>= \τ -> case τ of
   T.UnionTy _ _ -> excluded τ
   T.LitTy _ -> excluded τ
   T.CallableTy _ _ -> excluded τ
   _ -> pure (T.TypeTy τ)
   where
   excluded τ = throwError $ "Excluded from type[...]: " <> prettyP τ
resolveType cxt (S.UnionTy ψ ψ') = T.UnionTy <$> resolveType cxt ψ <*> resolveType cxt ψ'

typeArguments :: Cxt -> Name -> List S.TypeExpr -> CheckM (List T.Type)
typeArguments cxt c ψs = do
   τs <- traverse (resolveType cxt) ψs
   αs <- gets \{ classes } -> (get c classes).typeParams
   when (length τs /= length αs) $ throwError $ arityMismatch "Class" c αs τs
   pure τs

arityMismatch :: String -> Name -> List Var -> List T.Type -> String
arityMismatch what q αs τs =
   what <> " " <> dottedName q <> " expects " <> show (length αs) <> " type argument(s); got " <> show (length τs)

instance WellFormed S.Expr E.Expr where
   wellFormed cxt (S.Var x) = E.Var x <$ var cxt x
   wellFormed _ (S.Lit ℓ) = pure (E.Lit ℓ)
   wellFormed cxt (S.Call e ψs es xes) = case asName e >>= \c -> (c × _) <$> resolveName cxt c of
      Just (c × Type name) -> do
         for_ (NEL.fromList ψs) \_ -> resolveType cxt (S.NameTy c ψs)
         xs <- gets \{ classes } -> fieldNames classes name
         when (null xes && length es /= length xs)
            $ throwError
            $ dottedName c <> " expects " <> show (length xs) <> " argument(s); got " <> show (length es)
         xes' <- if null xes then pure Nil else positionaliseKw xs c (length es) xes
         E.Constr name <$> traverse (wellFormed cxt) (es <> xes')
      _ -> case NEL.fromList ψs of
         Nothing -> do
            when (not (null xes)) $ throwError "Keyword arguments in function call"
            E.App <$> wellFormed cxt e <*> traverse (wellFormed cxt) es
         Just ψs' -> do
            i <- traverse asIndex ψs'
            wellFormed cxt (S.Call (S.Subscript e (indexExpr i)) Nil es xes)
      where
      asIndex (S.NameTy q Nil) = pure (nameExpr q)
      asIndex _ = throwError $ "Type arguments for non-class: " <> maybe "expression" dottedName (asName e)

      indexExpr (NonEmptyList (i :| Nil)) = i
      indexExpr is = S.Tuple (NEL.toList is)

      nameExpr q = foldl S.Attribute (S.Var (head q)) (tail q)
   wellFormed cxt (S.BinOp e op e') = E.BinOp <$> wellFormed cxt e <@> op <*> wellFormed cxt e'
   wellFormed cxt (S.UnOp op e) = E.UnOp op <$> wellFormed cxt e
   wellFormed cxt (S.And e e') = E.And <$> wellFormed cxt e <*> wellFormed cxt e'
   wellFormed cxt (S.Or e e') = E.Or <$> wellFormed cxt e <*> wellFormed cxt e'
   wellFormed cxt (S.InfixApp e f e') = do
      var cxt f
      E.App (E.Var f) <$> traverse (wellFormed cxt) (e : e' : Nil)
   wellFormed cxt (S.Cond e1 e e2) = E.Cond <$> wellFormed cxt e1 <*> wellFormed cxt e <*> wellFormed cxt e2
   wellFormed cxt (S.Attribute e y) = case resolveName cxt =<< asName e of
      Just (ModChecked q cxt') -> do
         when (not (y ∈ cxt'))
            $ throwError
            $ "module " <> dottedName q <> " has no member " <> y
         var cxt' y
         pure (E.ModMember q y)
      _ -> flip E.Attribute y <$> wellFormed cxt e
   wellFormed cxt (S.Subscript e e')
      | Just ψ <- typeValue cxt (S.Subscript e e') = E.Type <$> resolveType cxt ψ
      | otherwise = E.Subscript <$> wellFormed cxt e <*> wellFormed cxt e'
   wellFormed cxt (S.Matrix e1 (x × y) e2) =
      (\e2' e1' -> E.Matrix e1' (x × y) e2') <$> wellFormed cxt e2 <*> wellFormed
         (cxt `override` constMap Assigned (singleton x ∪ singleton y))
         e1
   wellFormed cxt (S.Lambda (S.LambdaClause (ps × e))) = do
      ps' <- traverse (wellFormed cxt) ps
      e' <- wellFormed (cxt `override` constMap Assigned (unions (bv <$> ps))) e
      E.Lambda <$> clauses (NEL.singleton ((ps' <#> (_ × Nothing)) × Nothing × E.Return e'))
   wellFormed cxt (S.Dictionary kvs) =
      E.Dictionary <$> traverse (\(k × v) -> Pair <$> wellFormed cxt k <*> wellFormed cxt v) kvs
   wellFormed cxt (S.Paragraph elems) =
      E.Constr cParagraph <<< (_ : Nil) <<< E.List <$> traverse pe elems
      where
      pe (S.Unquote e) = wellFormed cxt e
      pe (S.Token str) = pure (E.Lit (Str str))
   wellFormed cxt (S.List es) = E.List <$> traverse (wellFormed cxt) es
   wellFormed cxt (S.Tuple es) = E.Tuple <$> traverse (wellFormed cxt) es
   wellFormed cxt (S.ListComp e gs) = do
      notCaptured (bv gs) (captures e)
      (\(e' × gs') -> E.ListComp e' gs') <$> wellFormedQualifiers cxt gs (\cxt' -> wellFormed cxt' e)
   wellFormed cxt (S.DictComp k e gs) = do
      notCaptured (bv gs) (captures k ∪ captures e)
      (\((k' × e') × gs') -> E.DictComp k' e' gs') <$> wellFormedQualifiers cxt gs \cxt' ->
         (×) <$> wellFormed cxt' k <*> wellFormed cxt' e
   wellFormed cxt (S.DocExpr e e') =
      E.DocExpr <$> wellFormed (cxt `override` constMap Assigned (singleton varThis)) e <*> wellFormed cxt e'

typeValue :: Cxt -> S.Expr -> Maybe S.TypeExpr
typeValue cxt e@(S.Subscript e' _) = do
   q <- asName e'
   guard case resolveName cxt q, q of
      Just (Type _), _ -> true
      _, NonEmptyList (x :| Nil) -> lookup x cxt == Just PredefName && x `elem` [ "list", "dict", "tuple" ]
      _, _ -> false
   typeExprOf e
typeValue _ _ = Nothing

typeExprOf :: S.Expr -> Maybe S.TypeExpr
typeExprOf (S.Subscript e e') = do
   q <- asName e
   ψs <- case e' of
      S.Tuple es -> traverse typeExprOf es
      _ -> singleton <$> typeExprOf e'
   case dottedName q, ψs of
      "list", ψ : Nil -> pure (S.ListTy ψ)
      "dict", S.PrimitiveTy T.Str : ψ : Nil -> pure (S.DictTy ψ)
      "tuple", _ -> pure (S.TupleTy ψs)
      _, _ -> pure (S.NameTy q ψs)
typeExprOf e = asName e <#> \q -> case dottedName q of
   "None" -> S.PrimitiveTy T.None
   "object" -> S.PrimitiveTy T.Object
   "bool" -> S.PrimitiveTy T.Bool
   "int" -> S.PrimitiveTy T.Int
   "float" -> S.PrimitiveTy T.Float
   "str" -> S.PrimitiveTy T.Str
   "Never" -> S.PrimitiveTy T.Never
   "Sized" -> S.PrimitiveTy T.Sized
   _ -> S.NameTy q Nil

asName :: S.Expr -> Maybe Name
asName (S.Var x) = Just (singleton x)
asName (S.Attribute e y) = asName e <#> (_ <> singleton y)
asName _ = Nothing

wellFormedQualifiers
   :: forall b
    . Cxt
   -> List S.Qualifier
   -> (Cxt -> CheckM b)
   -> CheckM (b × List E.Qualifier)
wellFormedQualifiers cxt Nil body = (_ × Nil) <$> body cxt
wellFormedQualifiers cxt (g : gs) body = case g of
   S.Guard e -> do
      e' <- wellFormed cxt e
      map (E.Guard e' : _) <$> wellFormedQualifiers cxt gs body
   S.Generator p e -> do
      notCaptured (bv p) (captures gs)
      e' <- wellFormed cxt e
      p' <- wellFormed cxt p
      map (E.Generator p' e' : _) <$> wellFormedQualifiers (cxt `override` constMap Assigned (bv p)) gs body
   S.Decl (S.VarDef p ψ e) -> do
      notCaptured (bv p) (captures gs)
      for_ ψ (resolveType cxt)
      e' <- wellFormed cxt e
      p' <- wellFormed cxt p
      map (E.Decl p' e' : _) <$> wellFormedQualifiers (cxt `override` constMap Assigned (bv p)) gs body

notCaptured :: Set Var -> Set Var -> CheckM Unit
notCaptured xs captured = for_ (xs ∩ captured) \x -> throwError $ "Comprehension variable captured by lambda: " <> x

-- Keyword arguments in field order; must cover fields xs after first n exactly.
positionaliseKw :: forall b. List Var -> Name -> Int -> List (Bind b) -> CheckM (List b)
positionaliseKw xs c n xbs = do
   when (sort provided /= sort remaining) $ throwError $
      "Class " <> last c <> " keyword fields mismatch: expected " <> show remaining <> ", got " <> show provided
   pure $ remaining <#> \f -> definitely' (snd <$> find (\(k ↦ _) -> k == f) xbs)
   where
   remaining = drop n xs
   provided = fst <$> xbs

-- Parameter names for desugared functions, kept apart from source identifiers by the leading $.
param :: Int -> Var
param i = "$" <> show i

wellFormedClause :: Cxt -> S.Clause -> CheckM (List (S.Pattern × Maybe T.Type) × Maybe T.Type × E.Stmt)
wellFormedClause cxt (S.Clause (αs × ps × ψ × s)) = do
   let cxt_αs = cxt `override` typeParams αs
   let xs = unions (bv <$> ps)
   ps' <- for ps \(S.Param p ψ') -> (×) <$> wellFormed cxt p <*> traverse (resolveType cxt_αs) ψ'
   τ <- traverse (resolveType cxt_αs) ψ
   cxt_s <- scope "parameter" xs s
   r × s' <- wellFormed (cxt_αs `override` constMap Assigned xs `override` cxt_s) s
   pure (ps' × τ × close r s')
   where
   -- Body that may fall through returns None
   close Returns s' = s'
   close (Assigns _) s' = E.Seq s' (E.Return (E.Lit None))

-- Clauses over k parameters as a function of k parameters. A parameter column that is the same variable in
-- every clause is a parameter of that name; the remaining columns are matched together, as nested pairs when
-- there are several.
clauses :: NonEmptyList (List (S.Pattern × Maybe T.Type) × Maybe T.Type × E.Stmt) -> CheckM E.Def
clauses cs = do
   let n = length (fst (head cs)) :: Int
   for_ cs \(ps × _) ->
      when (length ps /= n) $ throwError "Clauses differ in number of parameters"
   for_ (transpose (NEL.toList (cs <#> \(ps × _) -> snd <$> ps))) (agree "parameter annotations" <<< nonEmpty)
   agree "return annotation" (cs <#> \(_ × ψ × _) -> ψ)
   let
      columns = transpose (NEL.toList (cs <#> \(ps × _) -> fst <$> ps))
      named = columns # mapWithIndex \i ps -> case sharedVar ps of
         Just x -> x × Nothing
         Nothing -> param (i + 1) × Just ps
      matched = named # mapMaybe \(x × ps_opt) -> (x × _) <$> ps_opt
      ss = cs <#> \(_ × _ × s) -> s
      body = case matched of
         Nil -> head ss
         (x × ps) : Nil -> E.Match (E.Var x) (zip (nonEmpty ps) ss)
         _ -> E.Match (E.Tuple (E.Var <<< fst <$> matched))
            (zipWith (\ps s -> S.PTuple ps × s) (nonEmpty (transpose (snd <$> matched))) ss)
   pure (E.Def (fst <$> named) body)
   where
   sharedVar :: List S.Pattern -> Maybe Var
   sharedVar (S.PVar x : ps) | all (_ == S.PVar x) ps = Just x
   sharedVar _ = Nothing

   agree :: String -> NonEmptyList (Maybe T.Type) -> CheckM Unit
   agree what ψs =
      unless (all (\ψ -> ψ == Nothing || ψ == head ψs) (tail ψs)) $ throwError ("Clauses differ in " <> what)

var :: Cxt -> Var -> CheckM Unit
var cxt x = case lookup x cxt of
   Just Assigned -> pure unit
   Just (Type _) -> pure unit
   Just Unbound -> throwError $ "Declared later in scope: " <> x
   Just Declared -> throwError $ "Declared but not initialised: " <> x
   Just PossiblyUnassigned -> throwError $ "Possibly uninitialised: " <> x
   Just (Mod q) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just (ModChecked q _) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just θ -> throwError $ kind θ <> " " <> x <> " is not a value"
   Nothing -> throwError $ "Unbound name: " <> x

-- Unbound accepted until annotations are required, as an unannotated assignment also declares.
assignable :: Cxt -> Var -> CheckM Unit
assignable cxt x = case lookup x cxt of
   Just Declared -> pure unit
   Just Unbound -> pure unit
   Just Assigned -> throwError $ "Already initialised: " <> x
   Just PossiblyUnassigned -> throwError $ "Possibly initialised already: " <> x
   Just θ -> throwError $ "Reassigned " <> kind θ <> " name: " <> x
   Nothing -> error absurd

kind :: Entry -> String
kind (Type _) = "class"
kind (TypeAlias _ _) = "type alias"
kind (Mod _) = "module"
kind (ModChecked _ _) = "module"
kind PredefName = "predefined name"
kind TypeVar = "type parameter"
kind _ = "variable"

predefName :: Cxt -> Var -> CheckM Unit
predefName cxt x = case lookup x cxt of
   Just PredefName -> pure unit
   _ -> throwError $ "Not bound as a predefined name: " <> x

-- Each case pattern well-formed and not subsumed by an earlier one.
wellFormedPatterns :: Cxt -> NonEmptyList S.Pattern -> CheckM (NonEmptyList S.Pattern)
wellFormedPatterns cxt ps = do
   classes <- gets _.classes
   forWithIndex ps \i p -> do
      forWithIndex_ (drop (i + 1) (NEL.toList ps)) \j p' ->
         when (subsumed classes cxt p' p) $ throwError $ "case " <> show (i + j + 2) <> " is unreachable"
      wellFormed cxt p

instance WellFormed S.Pattern S.Pattern where
   wellFormed cxt (S.PConstr c ps xps) = do
      name <- classOf cxt c
      when (name == cRange) $ throwError "range not permitted in a constructor pattern"
      xs <- gets \{ classes } -> fieldNames classes name
      when (null xps && length ps /= length xs)
         $ throwError
         $ dottedName c <> " expects " <> show (length xs) <> " argument(s); got " <> show (length ps)
      distinctVars (ps <> (snd <$> xps))
      xps' <- if null xps then pure Nil else positionaliseKw xs c (length ps) xps
      S.PConstr name <$> traverse (wellFormed cxt) (ps <> xps') <@> Nil
   wellFormed cxt (S.PRecord xps) = do
      checkDistinct ("Duplicate key in pattern: " <> _) (fst <$> xps)
      distinctVars (snd <$> xps)
      S.PRecord <$> traverse (traverse (wellFormed cxt)) xps
   wellFormed cxt (S.PList ps) = do
      distinctVars ps
      S.PList <$> traverse (wellFormed cxt) ps
   wellFormed cxt (S.PTuple ps) = do
      distinctVars ps
      S.PTuple <$> traverse (wellFormed cxt) ps
   wellFormed cxt (S.PAs p x) = do
      distinctVars (p : S.PVar x : Nil)
      S.PAs <$> wellFormed cxt p <@> x
   wellFormed _ p = pure p

distinctVars :: List S.Pattern -> CheckM Unit
distinctVars ps = checkDistinct ("Duplicate variable in pattern: " <> _) (ps >>= Set.toUnfoldable <<< bv)

subsumed :: ClassTable -> Cxt -> S.Pattern -> S.Pattern -> Boolean
subsumed _ _ _ (S.PVar _) = true
subsumed _ _ _ S.PWild = true
subsumed classes cxt (S.PAs p _) p' = subsumed classes cxt p p'
subsumed classes cxt p (S.PAs p' _) = subsumed classes cxt p p'
subsumed _ _ (S.PLit ℓ) (S.PLit ℓ') = ℓ == ℓ'
subsumed classes cxt (S.PRecord xps) (S.PRecord xps') =
   all (\(x × p') -> maybe false (\p -> subsumed classes cxt p p') (F.lookup x xps)) xps'
subsumed classes cxt (S.PList ps) (S.PList ps') = length ps == length ps' && and (zipWith (subsumed classes cxt) ps ps')
subsumed classes cxt (S.PTuple ps) (S.PTuple ps') = length ps == length ps' && and (zipWith (subsumed classes cxt) ps ps')
subsumed classes cxt (S.PConstr c ps xps) (S.PConstr c' ps' xps') = fromMaybe false do
   name <- hush (classOf cxt c)
   name' <- hush (classOf cxt c')
   guard (name' `elem` ancestors classes name)
   pure $ all (\x -> fromMaybe false (subsumed classes cxt <$> fieldMap classes name ps xps x <*> fieldMap classes name' ps' xps' x))
      (fst <$> fields classes name')
subsumed _ _ _ _ = false
