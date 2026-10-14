module WellFormed where

import Prelude hiding (absurd, join)

import Bind (Bind, Name, Var, dottedName, prefixOf, properPrefixOf, varThis, (↦))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.State (StateT, gets, mapStateT, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Control.MonadPlus (guard)
import Data.Foldable (all, and, elem, find, foldM, foldl, for_)
import Data.Function (on)
import Data.FoldableWithIndex (forWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.TraversableWithIndex (forWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.List (List(..), drop, filter, index, take, intersect, length, mapMaybe, null, sort, transpose, updateAt, (:))
import Data.Foldable (any, lookup) as F
import ModuleGraph (ModuleName, implicitFor, submodules)
import Data.List.NonEmpty (NonEmptyList(..), head, last, snoc, unsnoc)
import Data.NonEmpty ((:|))
import Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (fst, snd)
import DataType (ClassTable, Instance(..), ancestors, cMatrix, cParagraph, cRange, elemType, equiv, fieldMap, fieldNames, fields, fieldsAt, instanceAt, join, joins, meet, subtype, typeArgs)
import DefiniteAssignment (Entry(..), Cxt, WfResult(..), classOf, extend, mergeRes, override, overrideRes, resolveName)
import Dict as D
import Util.Map (constMap, get, insert, keys, lookup, maplet)
import Expr (Unop(..), bv, fv)
import Expr (Pattern(..)) as S
import Expr (Branch(..), Case, Def(..), Expr(..), Import(..), Module(..), Qualifier(..), RecDefs(..), Stmt(..)) as E
import Literal (Literal(..))
import Operator (binopSymbol, resolveBinop, resolveUnop, unopSymbol)
import Pretty (prettyP)
import SExpr (Branch, Clause(..), Expr(..), Import(..), LambdaClause(..), Module(..), Param(..), ParagraphElem(..), Qualifier(..), Stmt(..), TypeExpr(..), VarDef(..), assigns, binds, declares) as S
import Types (Scheme(..), bool, int, intPair, never, none, object, str)
import Types as T
import Util (MayFail, type (+), type (×), absurd, checkDistinct, definitely', error, nonEmpty, singleton, tail, zip, zipWith, (×), (∩))
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

classTable :: CheckM ClassTable
classTable = gets _.classes

checkProgram :: List S.Import -> S.Stmt -> CheckM E.Stmt
checkProgram imports s = do
   cxt_imp × cxt_ιs <- checkImports mainModule imports
   snd <$> (moduleScope cxt_imp cxt_ιs s >>= \cxt_s -> wellFormedTop mainModule cxt_s s)

-- Member context of module q, checked on first import; no cycle guard, as the dependency graph is acyclic.
checkModule :: ModuleName -> CheckM Cxt
checkModule q = gets _.modules >>= \modules -> case lookup q modules of
   Just { cxt } -> pure cxt
   Nothing -> within ("module " <> dottedName q) do
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

within :: forall a. String -> CheckM a -> CheckM a
within what = mapStateT (mapReaderT (lmap (_ <> "\nChecking " <> what)))

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
      Just (Declared _) -> throwError $ "Not definitely initialised: " <> x
      Just (PossiblyUnassigned _) -> throwError $ "Not definitely initialised: " <> x
      Just θ -> pure (insert x θ othersCxt)
      Nothing -> throwError $ "Cannot import name " <> x <> " from module " <> dottedName q

checkStatements :: Name -> Cxt -> Cxt -> S.Module -> CheckM (Cxt × E.Module)
checkStatements q cxt_imp cxt_ιs (S.Module imports s) = do
   r × s' <- moduleScope cxt_imp cxt_ιs s >>= \cxt_s -> wellFormedTop q cxt_s s
   case r of
      -- Program may return, with the returned value as its result; spec treats __main__ as a module
      Returns -> throwError "Module body cannot return"
      Assigns cxt' -> pure (insert "__name__" (Assigned str) cxt' × E.Module imports' s')
   where
   imports' = imports <#> \(S.Import q' xs) -> E.Import q' xs

moduleScope :: Cxt -> Cxt -> S.Stmt -> CheckM Cxt
moduleScope cxt_imp cxt_ιs s =
   insert "__name__" (Assigned str) <<< (cxt_imp `extend` cxt_ιs `override` _) <$> scope "imported name" (keys cxt_ιs) s

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

checkStmt :: Cxt -> Maybe T.Type -> S.Stmt -> CheckM (WfResult Cxt × E.Stmt)
checkStmt _ _ S.Pass = pure (Assigns empty × E.Pass)
-- Outside a function, the result of the program
checkStmt cxt ζ (S.Return e) = (Returns × _) <<< E.Return <$> maybe (exprStmt cxt e) (checkExpr cxt e) ζ
checkStmt cxt _ (S.ExprStmt e) = (Assigns empty × _) <<< E.ExprStmt <$> exprStmt cxt e
checkStmt cxt (Just _) (S.Assert (S.Lit (Bool false)) e') =
   (Returns × _) <$> (E.Assert (E.Lit (Bool false)) <$> traverse (\e'' -> checkExpr cxt e'' str) e')
checkStmt cxt _ (S.Assert e e') =
   (Assigns empty × _) <$> (E.Assert <$> checkExpr cxt e bool <*> traverse (\e'' -> checkExpr cxt e'' str) e')
checkStmt cxt _ (S.Declare x ψ) = do
   τ <- resolveType cxt ψ
   pure (Assigns (maplet x (Declared τ)) × E.Declare x)
checkStmt cxt _ (S.Def (S.VarDef (S.PVar x) (Just ψ) e)) = do
   assignable cxt x
   τ <- resolveType cxt ψ
   e' <- checkExpr cxt e τ
   pure (Assigns (maplet x (Assigned τ)) × E.Assign (S.PVar x) e')
checkStmt cxt _ (S.Def (S.VarDef (S.PVar x) Nothing e)) = case lookup x cxt of
   Just (Declared τ) -> do
      e' <- checkExpr cxt e τ
      pure (Assigns (maplet x (Assigned τ)) × E.Assign (S.PVar x) e')
   Just Unbound -> throwError $ "Assigned before declaration: " <> x
   _ -> assignable cxt x >>= \_ -> error absurd
-- Assignment to a pattern; the spec has only variables
checkStmt cxt _ (S.Def (S.VarDef p ψ e)) = do
   for_ (bv p) (unbound cxt)
   τ × e' <- annotated cxt ψ e
   p' <- pattern cxt p
   cxt_p <- checkPattern p' τ
   pure (Assigns (bindings cxt_p) × E.Assign p' e')
checkStmt cxt _ (S.DefRec ds) = do
   let defs = NEL.groupBy (eq `on` fst) ds
   checkDistinct ("Non-contiguous clauses for: " <> _) (NEL.toList (fst <<< head <$> defs))
   fπs <- for defs \def -> (fst (head def) × _) <$> defSignature cxt (head def)
   let cxt_fs = Map.fromFoldable (fπs <#> \(f × π) -> f × either Assigned TypeScheme π)
   let cxt' = cxt `override` cxt_fs
   defs' <- for (NEL.zip defs fπs) \(def × (f × π)) -> do
      void $ wellFormedPatterns cxt' (def <#> \(_ × S.Clause (_ × ps × _)) -> S.PList (ps <#> \(S.Param p _) -> p))
      within ("definition of " <> f) $ (f ↦ _) <$> (traverse (defBody cxt' f (either identity (\(Forall _ σ) -> σ) π) <<< snd) def >>= clauses)
   pure (Assigns cxt_fs × E.DefRec (E.RecDefs (D.fromFoldable defs')))
checkStmt cxt ζ (S.Seq s1 s2) = do
   r1 × s1' <- checkStmt cxt ζ s1
   case r1 of
      Returns -> throwError "Unreachable statement"
      Assigns cxt' -> do
         r2 × s2' <- checkStmt (cxt `override` cxt') ζ s2
         pure (overrideRes r1 r2 × E.Seq s1' s2')
checkStmt cxt ζ (S.If es elseBranch) = do
   es' <- for es \(e × s) -> do
      e' <- checkExpr cxt e bool
      r × s' <- checkStmt cxt ζ s
      pure (r × E.Branch e' s')
   rElse × elseBranch' <- case elseBranch of
      Just s -> map Just <$> checkStmt cxt ζ s
      Nothing -> pure (Assigns empty × Nothing)
   pure (foldl1 mergeRes (NEL.cons rElse (fst <$> es')) × E.If (snd <$> es') elseBranch')
checkStmt cxt ζ (S.Match e bs) = do
   σ × e' <- synthExpr cxt e
   ps' <- wellFormedPatterns cxt (fst <$> bs)
   rbs × τ <- matchCases cxt ζ σ (NEL.toList (NEL.zip ps' (snd <$> bs)))
   let rNoMatch = if τ == never then Returns else Assigns empty
   pure (foldl mergeRes rNoMatch (fst <$> rbs) × E.Match e' (nonEmpty (snd <$> rbs)))
checkStmt _ _ (S.Dataclass _ _ _ _) = error absurd
checkStmt _ _ (S.TypeAlias _ _ _) = error absurd

matchCases :: Cxt -> Maybe T.Type -> T.Type -> List (S.Pattern × S.Stmt) -> CheckM (List (WfResult Cxt × E.Case) × T.Type)
matchCases _ _ τ Nil = pure (Nil × τ)
matchCases cxt ζ τ ((p × s) : bs) = do
   for_ (bv p) (unbound cxt)
   cxt_p <- checkPattern p τ
   r × s' <- checkStmt (cxt `override` bindings cxt_p) ζ s
   τ' <- classTable <#> \classes -> remaining classes τ p
   lmap ((overrideRes (Assigns (bindings cxt_p)) r × (p × s')) : _) <$> matchCases cxt ζ τ' bs

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
wellFormedTop _ cxt s = checkStmt cxt Nothing s

typeParams :: List Var -> Cxt
typeParams αs = constMap TypeVar (Set.fromFoldable αs)

bindings :: Map.Map Var T.Type -> Cxt
bindings = map Assigned

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

typeArgsFor :: List Var -> Map.Map Var T.Type -> CheckM (List T.Type)
typeArgsFor αs γ = for αs \α -> maybe (throwError $ "Type parameter " <> α <> " has no type argument") pure (lookup α γ)

annotated :: Cxt -> Maybe S.TypeExpr -> S.Expr -> CheckM (T.Type × E.Expr)
annotated cxt (Just ψ) e = resolveType cxt ψ >>= \τ -> (τ × _) <$> checkExpr cxt e τ
annotated cxt Nothing e = synthExpr cxt e

synthExpr :: Cxt -> S.Expr -> CheckM (T.Type × E.Expr)
synthExpr cxt (S.Var x) = (_ × E.Var x) <$> varType cxt x
synthExpr _ (S.Lit ℓ) = pure (T.LitTy ℓ × E.Lit ℓ)
synthExpr cxt (S.Call e ψs es xes) = case className cxt e of
   Just (c × name) -> constr cxt c name ψs es xes
   Nothing -> case NEL.fromList ψs of
      Nothing -> do
         when (not (null xes)) $ throwError "Keyword arguments in call other than constructor call"
         case e of
            S.Lambda (S.LambdaClause (ps × e')) -> do
               cxt_p × ps' × es' <- lambdaArgs cxt ps es
               τ × e'' <- synthExpr (cxt `override` bindings cxt_p) e'
               (τ × _) <<< flip E.App es' <$> lambda ps' e''
            _ -> do
               σ × e' <- callee cxt e es
               τ × es' <- resultType cxt σ es
               pure (τ × E.App e' es')
      Just ψs' -> do
         i <- traverse asIndex ψs'
         synthExpr cxt (S.Call (S.Subscript e (indexExpr i)) Nil es xes)
   where
   asIndex (S.NameTy q Nil) = pure (qualified q)
   asIndex _ = throwError $ "Type arguments for non-class: " <> maybe "expression" dottedName (asName e)

   indexExpr (NonEmptyList (i :| Nil)) = i
   indexExpr is = S.Tuple (NEL.toList is)

   qualified q = foldl S.Attribute (S.Var (head q)) (tail q)
synthExpr cxt (S.BinOp e op e') = do
   σ × e1 <- synthExpr cxt e
   σ' × e2 <- synthExpr cxt e'
   classes <- classTable
   τ <- maybe (throwError $ "No overload of " <> binopSymbol op <> " at operand types " <> prettyP σ <> " and " <> prettyP σ')
      pure
      (resolveBinop classes op σ σ')
   pure (τ × E.BinOp e1 op e2)
synthExpr _ (S.UnOp Neg (S.Lit (Int n))) = pure (T.LitTy (Int (-n)) × E.UnOp Neg (E.Lit (Int n)))
synthExpr _ (S.UnOp Neg (S.Lit (Float n))) = pure (T.LitTy (Float (-n)) × E.UnOp Neg (E.Lit (Float n)))
synthExpr cxt (S.UnOp op e) = do
   σ × e' <- synthExpr cxt e
   classes <- classTable
   τ <- maybe (throwError $ "No overload of " <> unopSymbol op <> " at operand type " <> prettyP σ) pure
      (resolveUnop classes op σ)
   pure (τ × E.UnOp op e')
synthExpr cxt (S.And e e') = (bool × _) <$> (E.And <$> checkExpr cxt e bool <*> checkExpr cxt e' bool)
synthExpr cxt (S.Or e e') = (bool × _) <$> (E.Or <$> checkExpr cxt e bool <*> checkExpr cxt e' bool)
synthExpr cxt (S.InfixApp e f e') = synthExpr cxt (S.Call (S.Var f) Nil (e : e' : Nil) Nil)
synthExpr cxt (S.Cond e1 e e2) = do
   e' <- checkExpr cxt e bool
   τ × es' <- joinSynth cxt identity (e1 : e2 : Nil)
   case es' of
      e1' : e2' : Nil -> pure (τ × E.Cond e1' e' e2')
      _ -> error absurd
synthExpr cxt (S.Attribute e y) = case resolveName cxt =<< asName e of
   Just (ModChecked q cxt') -> do
      when (not (y ∈ cxt'))
         $ throwError
         $ "module " <> dottedName q <> " has no member " <> y
      (_ × E.ModMember q y) <$> varType cxt' y
   _ -> do
      σ × e' <- synthExpr cxt e
      classes <- classTable
      (_ × E.Attribute e' y) <$> attributeType classes σ
   where
   attributeType classes (T.UnionTy σ σ') = join classes <$> attributeType classes σ <*> attributeType classes σ'
   attributeType classes (T.ClassTy c τs) =
      maybe (throwError $ "Class " <> last c <> " has no field " <> y) pure (F.lookup y (fieldsAt classes c τs))
   attributeType _ σ = throwError $ "Values of type " <> prettyP σ <> " have no attributes"
synthExpr cxt e@(S.Subscript e1 e2)
   | Just ψ <- typeValue cxt e = resolveType cxt ψ <#> \τ -> T.TypeTy τ × E.Type τ
   | otherwise = do
        σ × e1' <- synthExpr cxt e1
        τ × e2' <- subscriptType cxt σ e2
        pure (τ × E.Subscript e1' e2')
synthExpr cxt (S.Matrix e1 (x × y) e2) = do
   e2' <- checkExpr cxt e2 intPair
   τ × e1' <- synthExpr (cxt `override` bindings (Map.fromFoldable [ x × int, y × int ])) e1
   pure (T.ClassTy cMatrix (T.baseType τ : Nil) × E.Matrix e1' (x × y) e2')
synthExpr _ (S.Lambda _) = throwError notSynthesised
synthExpr cxt (S.Dictionary kvs) = do
   ks' <- for kvs \(k × _) -> checkExpr cxt k str
   τ × vs' <- joinSynth cxt T.baseType (snd <$> kvs)
   pure (T.DictTy τ × E.Dictionary (zipWith Pair ks' vs'))
synthExpr cxt (S.Paragraph elems) =
   (T.ClassTy cParagraph Nil × _) <<< E.Constr cParagraph <<< (_ : Nil) <<< E.List <$> traverse pe elems
   where
   pe (S.Unquote e) = snd <$> synthExpr cxt e
   pe (S.Token str') = pure (E.Lit (Str str'))
synthExpr cxt (S.List es) = lmap T.ListTy <<< map E.List <$> joinSynth cxt T.baseType es
synthExpr cxt (S.Tuple es) = do
   τes <- traverse (synthExpr cxt) es
   pure (T.TupleTy (fst <$> τes) × E.Tuple (snd <$> τes))
synthExpr cxt (S.ListComp e gs) = do
   notCaptured (bv gs) (captures e)
   (τ × e') × gs' <- checkQuals cxt gs \cxt' -> synthExpr cxt' e
   pure (T.ListTy (T.baseType τ) × E.ListComp e' gs')
synthExpr cxt (S.DictComp k e gs) = do
   notCaptured (bv gs) (captures k ∪ captures e)
   (k' × τ × e') × gs' <- checkQuals cxt gs \cxt' -> (×) <$> checkExpr cxt' k str <*> synthExpr cxt' e
   pure (T.DictTy (T.baseType τ) × E.DictComp k' e' gs')
synthExpr cxt (S.DocExpr e e') = do
   τ × e2 <- synthExpr cxt e'
   _ × e1 <- synthExpr (cxt `override` bindings (Map.singleton varThis τ)) e
   pure (τ × E.DocExpr e1 e2)

checkExpr :: Cxt -> S.Expr -> T.Type -> CheckM E.Expr
checkExpr cxt (S.Call (S.Lambda (S.LambdaClause (ps × e))) Nil es Nil) τ = do
   cxt_p × ps' × es' <- lambdaArgs cxt ps es
   e' <- checkExpr (cxt `override` bindings cxt_p) e τ
   flip E.App es' <$> lambda ps' e'
checkExpr cxt (S.List es) (T.ListTy τ) = E.List <$> traverse (\e -> checkExpr cxt e τ) es
checkExpr cxt (S.Tuple es) (T.TupleTy τs)
   | length es == length τs = E.Tuple <$> sequence (zipWith (checkExpr cxt) es τs)
checkExpr cxt (S.Dictionary kvs) (T.DictTy τ) =
   E.Dictionary <$> for kvs \(k × v) -> Pair <$> checkExpr cxt k str <*> checkExpr cxt v τ
checkExpr cxt (S.Lambda (S.LambdaClause (ps × e))) τ = case τ of
   T.CallableTy σs τ' | length σs == length ps -> do
      distinctVars ps
      ps' <- traverse (pattern cxt) ps
      cxt_p <- sequence (zipWith checkPattern ps' σs) <#> foldl (∪) empty
      e' <- checkExpr (cxt `override` bindings cxt_p) e τ'
      lambda ps' e'
   _ -> throwError $ "Expected type " <> prettyP τ <> "; given lambda with " <> show (length ps) <> " parameter(s)"
checkExpr cxt e τ | Just (αs × σ) <- schemeOf cxt e = do
   classes <- classTable
   τs <- typeArgsFor αs (typeArgs classes αs σ τ)
   let σ' = T.subst τs αs σ
   unless (subtype classes σ' τ) $ throwError $ typeMismatch τ σ'
   nameExpr cxt e
checkExpr cxt (S.Cond e1 e e2) τ = E.Cond <$> checkExpr cxt e1 τ <*> checkExpr cxt e bool <*> checkExpr cxt e2 τ
checkExpr cxt (S.ListComp e gs) (T.ListTy τ) = do
   notCaptured (bv gs) (captures e)
   (\(e' × gs') -> E.ListComp e' gs') <$> checkQuals cxt gs \cxt' -> checkExpr cxt' e τ
checkExpr cxt (S.DictComp k e gs) (T.DictTy τ) = do
   notCaptured (bv gs) (captures k ∪ captures e)
   (\((k' × e') × gs') -> E.DictComp k' e' gs') <$> checkQuals cxt gs \cxt' ->
      (×) <$> checkExpr cxt' k str <*> checkExpr cxt' e τ
checkExpr cxt (S.Matrix e1 (x × y) e2) (T.ClassTy c (τ : Nil)) | c == cMatrix = do
   e2' <- checkExpr cxt e2 intPair
   e1' <- checkExpr (cxt `override` bindings (Map.fromFoldable [ x × int, y × int ])) e1 τ
   pure (E.Matrix e1' (x × y) e2')
checkExpr cxt (S.DocExpr e e') τ = do
   e2 <- checkExpr cxt e' τ
   _ × e1 <- synthExpr (cxt `override` bindings (Map.singleton varThis τ)) e
   pure (E.DocExpr e1 e2)
checkExpr cxt e τ = do
   σ × e' <- synthExpr cxt e
   classes <- classTable
   unless (subtype classes σ τ) $ throwError $ typeMismatch τ σ
   pure e'

typeMismatch :: T.Type -> T.Type -> String
typeMismatch τ σ = "Expected type " <> prettyP τ <> "; given " <> prettyP σ

notSynthesised :: String
notSynthesised = "Cannot infer type of expression"

-- Whether some rule synthesises a type for e.
synthesises :: S.Expr -> Boolean
synthesises (S.Lambda _) = false
synthesises (S.Cond e1 _ e2) = synthesises e1 || synthesises e2
synthesises (S.List es) = F.any synthesises es
synthesises (S.Dictionary kvs) = F.any (synthesises <<< snd) kvs
synthesises (S.Tuple es) = all synthesises es
synthesises (S.ListComp e _) = synthesises e
synthesises (S.DictComp _ e _) = synthesises e
synthesises (S.Matrix e _ _) = synthesises e
synthesises (S.Call (S.Lambda (S.LambdaClause (_ × e))) _ es _) = synthesises e && all synthesises es
synthesises _ = true

-- Join of types (under f) of expressions which synthesise, against which the others are checked.
joinSynth :: Cxt -> (T.Type -> T.Type) -> List S.Expr -> CheckM (T.Type × List E.Expr)
joinSynth cxt f es = do
   results <- for es \e -> if synthesises e then Right <$> synthExpr cxt e else pure (Left e)
   let τs = results >>= either (const Nil) (pure <<< f <<< fst)
   when (null τs) $ throwError notSynthesised
   τ <- classTable <#> \classes -> joins classes τs
   (τ × _) <$> for results (either (\e -> checkExpr cxt e τ) (pure <<< snd))

subscriptType :: Cxt -> T.Type -> S.Expr -> CheckM (T.Type × E.Expr)
subscriptType cxt σ e | T.baseType σ == str = (str × _) <$> checkExpr cxt e int
subscriptType cxt (T.UnionTy σ σ') e = do
   τ × e' <- subscriptType cxt σ e
   τ' × _ <- subscriptType cxt σ' e
   classTable <#> \classes -> join classes τ τ' × e'
subscriptType cxt (T.ListTy τ) e = (τ × _) <$> checkExpr cxt e int
subscriptType cxt (T.DictTy τ) e = (τ × _) <$> checkExpr cxt e str
subscriptType cxt (T.TupleTy τs) e = do
   σ × e' <- synthExpr cxt e
   case σ of
      T.LitTy (Int n)
         | 0 <= n && n < m -> pure (definitely' (τs `index` n) × e')
         | -m <= n && n < 0 -> pure (definitely' (τs `index` (m + n)) × e')
         | otherwise -> throwError $ "Index " <> show n <> " out of range for tuple of length " <> show m
      _ | σ == int -> classTable <#> \classes -> joins classes τs × e'
      _ -> throwError $ typeMismatch int σ
   where
   m = length τs
subscriptType cxt (T.ClassTy c (τ : Nil)) e | c == cMatrix = (τ × _) <$> checkExpr cxt e intPair
subscriptType _ σ _ = throwError $ "Values of type " <> prettyP σ <> " cannot be subscripted"

constr :: Cxt -> Name -> Name -> List S.TypeExpr -> List S.Expr -> List (Bind S.Expr) -> CheckM (T.Type × E.Expr)
constr cxt c name ψs es xes = do
   classes <- classTable
   let xs = fieldNames classes name
   when (null xes && length es /= length xs)
      $ throwError
      $ dottedName c <> " expects " <> show (length xs) <> " argument(s); got " <> show (length es)
   es' <- (es <> _) <$> if null xes then pure Nil else positionaliseKw xs c (length es) xes
   τs <- case NEL.fromList ψs of
      Just _ -> typeArguments cxt name ψs
      Nothing -> do
         let αs = (get name classes).typeParams
         σes <- for (filter (synthesises <<< snd) (zip (snd <$> fields classes name) es')) \(σ × e) ->
            (σ × _) <<< fst <$> synthExpr cxt e
         typeArgsFor αs (typeArgs classes αs (fst <$> σes) (snd <$> σes))
   (T.ClassTy name τs × _) <<< E.Constr name <$>
      sequence (zipWith (\e (_ × σ) -> checkExpr cxt e σ) es' (fieldsAt classes name τs))

callee :: Cxt -> S.Expr -> List S.Expr -> CheckM (T.Type × E.Expr)
callee cxt e es = case schemeOf cxt e of
   Just (αs × σ@(T.CallableTy σs _)) -> do
      classes <- classTable
      στs <- for (filter (synthesises <<< snd) (zip σs es)) \(σ' × e') -> (σ' × _) <<< fst <$> synthExpr cxt e'
      τs <- typeArgsFor αs (typeArgs classes αs (fst <$> στs) (snd <$> στs))
      (T.subst τs αs σ × _) <$> nameExpr cxt e
   _ -> synthExpr cxt e

resultType :: Cxt -> T.Type -> List S.Expr -> CheckM (T.Type × List E.Expr)
resultType cxt (T.UnionTy σ σ') es = do
   τ × es' <- resultType cxt σ es
   τ' × _ <- resultType cxt σ' es
   classTable <#> \classes -> join classes τ τ' × es'
-- Automatic currying (PurePy extension)
resultType cxt (T.CallableTy σs τ) es = do
   es' <- checkArgs cxt σs (take (length σs) es)
   case drop (length es) σs, drop (length σs) es of
      Nil, Nil -> do
         when (τ == none) $ throwError "Call returning None used as expression"
         pure (τ × es')
      Nil, es'' -> map (es' <> _) <$> resultType cxt τ es''
      σs', _ -> pure (T.CallableTy σs' τ × es')
resultType _ σ _ = throwError $ "Values of type " <> prettyP σ <> " cannot be called"

exprStmt :: Cxt -> S.Expr -> CheckM E.Expr
exprStmt cxt (S.Call e Nil es Nil) | not (isLambda e) && className cxt e == Nothing = callStmt cxt e es
exprStmt cxt e = snd <$> synthExpr cxt e

callStmt :: Cxt -> S.Expr -> List S.Expr -> CheckM E.Expr
callStmt cxt e es = callee cxt e es >>= case _ of
   T.CallableTy σs τ × e' | τ == none && length σs == length es -> E.App e' <$> checkArgs cxt σs es
   σ × e' -> E.App e' <<< snd <$> resultType cxt σ es

checkArgs :: Cxt -> List T.Type -> List S.Expr -> CheckM (List E.Expr)
checkArgs cxt σs es = sequence (zipWith (checkExpr cxt) es σs)

lambdaArgs :: Cxt -> List S.Pattern -> List S.Expr -> CheckM (Map.Map Var T.Type × List S.Pattern × List E.Expr)
lambdaArgs cxt ps es = do
   when (length ps /= length es)
      $ throwError
      $ "Call expects " <> show (length ps) <> " argument(s); given " <> show (length es)
   distinctVars ps
   ps' <- traverse (pattern cxt) ps
   τes <- traverse (synthExpr cxt) es
   cxt_p <- sequence (zipWith checkPattern ps' (fst <$> τes)) <#> foldl (∪) empty
   pure (cxt_p × ps' × (snd <$> τes))

lambda :: List S.Pattern -> E.Expr -> CheckM E.Expr
lambda ps e = E.Lambda <$> clauses (NEL.singleton ((ps <#> (_ × Nothing)) × Nothing × E.Return e))

isLambda :: S.Expr -> Boolean
isLambda (S.Lambda _) = true
isLambda _ = false

className :: Cxt -> S.Expr -> Maybe (Name × Name)
className cxt e = asName e >>= \c -> case resolveName cxt c of
   Just (Type name) -> Just (c × name)
   _ -> Nothing

schemeOf :: Cxt -> S.Expr -> Maybe (List Var × T.Type)
schemeOf cxt e = asName e >>= resolveName cxt >>= case _ of
   TypeScheme (Forall αs σ) -> Just (NEL.toList αs × σ)
   _ -> Nothing

nameExpr :: Cxt -> S.Expr -> CheckM E.Expr
nameExpr _ (S.Var x) = pure (E.Var x)
nameExpr cxt (S.Attribute e y) | Just (ModChecked q _) <- resolveName cxt =<< asName e = pure (E.ModMember q y)
nameExpr _ _ = error absurd

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

checkQuals
   :: forall b
    . Cxt
   -> List S.Qualifier
   -> (Cxt -> CheckM b)
   -> CheckM (b × List E.Qualifier)
checkQuals cxt Nil body = (_ × Nil) <$> body cxt
checkQuals cxt (g : gs) body = case g of
   S.Guard e -> do
      e' <- checkExpr cxt e bool
      map (E.Guard e' : _) <$> checkQuals cxt gs body
   S.Generator p e -> do
      notCaptured (bv p) (captures gs)
      σ × e' <- synthExpr cxt e
      classes <- classTable
      τ <- maybe (throwError $ "Values of type " <> prettyP σ <> " cannot be iterated") pure (elemType classes σ)
      p' <- pattern cxt p
      cxt_p <- checkPattern p' τ
      map (E.Generator p' e' : _) <$> checkQuals (cxt `override` bindings cxt_p) gs body
   S.Decl (S.VarDef p ψ e) -> do
      notCaptured (bv p) (captures gs)
      τ × e' <- annotated cxt ψ e
      p' <- pattern cxt p
      cxt_p <- checkPattern p' τ
      map (E.Decl p' e' : _) <$> checkQuals (cxt `override` bindings cxt_p) gs body

notCaptured :: Set Var -> Set Var -> CheckM Unit
notCaptured xs captured = for_ (xs ∩ captured) \x -> throwError $ "Comprehension variable captured by lambda: " <> x

-- Keyword arguments in field order; must cover fields xs after first n exactly.
positionaliseKw :: forall b. List Var -> Name -> Int -> List (Bind b) -> CheckM (List b)
positionaliseKw xs c n xbs = do
   when (sort provided /= sort remaining') $ throwError $
      "Class " <> last c <> " keyword fields mismatch: expected " <> show remaining' <> ", got " <> show provided
   pure $ remaining' <#> \f -> definitely' (snd <$> find (\(k ↦ _) -> k == f) xbs)
   where
   remaining' = drop n xs
   provided = fst <$> xbs

-- Parameter names for desugared functions, kept apart from source identifiers by the leading $.
param :: Int -> Var
param i = "$" <> show i

defSignature :: Cxt -> S.Branch -> CheckM (T.Type + T.Scheme)
defSignature cxt (_ × S.Clause (αs × ps × ψ × _)) =
   scheme <$> (T.CallableTy <$> traverse (\(S.Param _ ψ') -> resolveType cxt_αs ψ') ps <*> resolveType cxt_αs ψ)
   where
   cxt_αs = cxt `override` typeParams αs
   scheme σ = maybe (Left σ) (\αs' -> Right (Forall αs' σ)) (NEL.fromList αs)

defBody :: Cxt -> Var -> T.Type -> S.Clause -> CheckM (List (S.Pattern × Maybe T.Type) × Maybe T.Type × E.Stmt)
defBody cxt f (T.CallableTy σs τ) (S.Clause (αs × ps × ψ × s)) = do
   let cxt_αs = cxt `override` typeParams αs
   let xs = unions (bv <$> ps)
   when (length ps /= length σs) $ throwError "Clauses differ in number of parameters"
   ps' <- for ps \(S.Param p ψ') -> (×) <$> pattern cxt p <*> (Just <$> resolveType cxt_αs ψ')
   cxt_p <- sequence (zipWith checkPattern (fst <$> ps') σs) <#> foldl (∪) empty
   τ' <- Just <$> resolveType cxt_αs ψ
   cxt_s <- scope "parameter" xs s
   r × s' <- checkStmt (cxt_αs `override` bindings cxt_p `override` cxt_s) (Just τ) s
   classes <- classTable
   case r of
      Assigns _ | not (equiv classes τ none) ->
         throwError $ f <> " declares result type " <> prettyP τ <> " but does not always return"
      _ -> pure (ps' × τ' × close r s')
   where
   -- Body that may fall through returns None
   close Returns s' = s'
   close (Assigns _) s' = E.Seq s' (E.Return (E.Lit None))
defBody _ _ _ _ = error absurd

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

varType :: Cxt -> Var -> CheckM T.Type
varType cxt x = case lookup x cxt of
   Just (Assigned τ) -> pure τ
   Just (TypeScheme _) -> throwError $ "Type arguments of " <> x <> " cannot be inferred"
   Just (Type c) -> do
      αs <- classTable <#> \classes -> (get c classes).typeParams
      unless (null αs) $ throwError $ "Type arguments of class " <> x <> " cannot be inferred"
      pure (T.TypeTy (T.ClassTy c Nil))
   Just Unbound -> throwError $ "Declared later in scope: " <> x
   Just (Declared _) -> throwError $ "Declared but not initialised: " <> x
   Just (PossiblyUnassigned _) -> throwError $ "Possibly uninitialised: " <> x
   Just (Mod q) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just (ModChecked q _) -> throwError $ "module " <> dottedName q <> " is not a value"
   Just θ -> throwError $ kind θ <> " " <> x <> " is not a value"
   Nothing -> throwError $ "Unbound name: " <> x

assignable :: Cxt -> Var -> CheckM Unit
assignable cxt x = case lookup x cxt of
   Just (Declared _) -> pure unit
   Just Unbound -> pure unit
   Just (Assigned _) -> throwError $ "Already initialised: " <> x
   Just (TypeScheme _) -> throwError $ "Already initialised: " <> x
   Just (PossiblyUnassigned _) -> throwError $ "Possibly initialised already: " <> x
   Just θ -> throwError $ "Reassigned " <> kind θ <> " name: " <> x
   Nothing -> error absurd

unbound :: Cxt -> Var -> CheckM Unit
unbound cxt x = case lookup x cxt of
   Just Unbound -> pure unit
   _ -> throwError $ "Already declared: " <> x

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
      pattern cxt p

-- Class patterns in positional form, by fully-qualified class name.
pattern :: Cxt -> S.Pattern -> CheckM S.Pattern
pattern cxt (S.PConstr c ps xps) = do
   name <- classOf cxt c
   when (name == cRange) $ throwError "range not permitted in a constructor pattern"
   xs <- gets \{ classes } -> fieldNames classes name
   when (null xps && length ps /= length xs)
      $ throwError
      $ dottedName c <> " expects " <> show (length xs) <> " argument(s); got " <> show (length ps)
   distinctVars (ps <> (snd <$> xps))
   xps' <- if null xps then pure Nil else positionaliseKw xs c (length ps) xps
   S.PConstr name <$> traverse (pattern cxt) (ps <> xps') <@> Nil
pattern cxt (S.PRecord xps) = do
   checkDistinct ("Duplicate key in pattern: " <> _) (fst <$> xps)
   distinctVars (snd <$> xps)
   S.PRecord <$> traverse (traverse (pattern cxt)) xps
pattern cxt (S.PList ps) = do
   distinctVars ps
   S.PList <$> traverse (pattern cxt) ps
pattern cxt (S.PTuple ps) = do
   distinctVars ps
   S.PTuple <$> traverse (pattern cxt) ps
pattern cxt (S.PAs p x) = do
   distinctVars (p : S.PVar x : Nil)
   S.PAs <$> pattern cxt p <@> x
pattern _ p = pure p

distinctVars :: List S.Pattern -> CheckM Unit
distinctVars ps = checkDistinct ("Duplicate variable in pattern: " <> _) (ps >>= Set.toUnfoldable <<< bv)

checkPattern :: S.Pattern -> T.Type -> CheckM (Map.Map Var T.Type)
checkPattern (S.PVar x) τ = pure (Map.singleton x τ)
checkPattern S.PWild _ = pure empty
checkPattern (S.PLit _) _ = pure empty
checkPattern (S.PAs p x) τ = do
   cxt_p <- checkPattern p τ
   classTable <#> \classes -> insert x (matchedAs classes τ p) cxt_p
checkPattern p@(S.PTuple ps) τ = do
   classes <- classTable
   when (hasListValues classes τ) $ throwError $ "Tuple pattern at " <> prettyP τ <> ", which has list values"
   bindingsAt τ p case _ of
      T.TupleTy τs | length τs == length ps -> Just (checkSeq ps τs)
      _ -> Nothing
checkPattern p@(S.PList ps) τ = do
   classes <- classTable
   when (hasTupleValues classes τ) $ throwError $ "List pattern at " <> prettyP τ <> ", which has tuple values"
   bindingsAt τ p case _ of
      T.ListTy σ -> Just (checkSeq ps (ps <#> const σ))
      _ -> Nothing
checkPattern p@(S.PRecord xps) τ = bindingsAt τ p case _ of
   T.DictTy σ -> Just (checkSeq (snd <$> xps) (xps <#> const σ))
   _ -> Nothing
checkPattern p@(S.PConstr c ps _) τ = do
   classes <- classTable
   bindingsAt τ p \σ -> case instanceAt classes c σ of
      Just Undetermined ->
         Just (throwError $ "Type arguments of " <> last c <> " in pattern not determined by type " <> prettyP σ)
      Just (Instance σs) -> Just (checkSeq ps (snd <$> fieldsAt classes c σs))
      Nothing -> Nothing

checkSeq :: List S.Pattern -> List T.Type -> CheckM (Map.Map Var T.Type)
checkSeq ps τs = sequence (zipWith checkPattern ps τs) <#> foldl (∪) empty

-- Bindings joined over disjuncts of τ; a disjunct not matched by p binds at Never.
bindingsAt :: T.Type -> S.Pattern -> (T.Type -> Maybe (CheckM (Map.Map Var T.Type))) -> CheckM (Map.Map Var T.Type)
bindingsAt τ p atDisjunct = do
   cxts <- for (T.disjuncts τ) \σ -> fromMaybe (pure (constMap never (bv p))) (atDisjunct σ)
   classTable <#> \classes -> foldl1 (Map.unionWith (join classes)) (nonEmpty cxts)

-- List types stand for every list type, as a disjunct other than a list type is above all list types or none.
hasListValues :: ClassTable -> T.Type -> Boolean
hasListValues classes τ = F.any (\σ -> isList σ || subtype classes (T.ListTy object) σ) (T.disjuncts τ)
   where
   isList (T.ListTy _) = true
   isList _ = false

hasTupleValues :: ClassTable -> T.Type -> Boolean
hasTupleValues classes τ = F.any (\σ -> isTuple σ || subtype classes (T.TupleTy Nil) σ) (T.disjuncts τ)
   where
   isTuple (T.TupleTy _) = true
   isTuple _ = false

matchedAs :: ClassTable -> T.Type -> S.Pattern -> T.Type
matchedAs classes (T.UnionTy σ σ') p = join classes (matchedAs classes σ p) (matchedAs classes σ' p)
matchedAs _ τ (S.PVar _) = τ
matchedAs _ τ S.PWild = τ
matchedAs classes τ (S.PAs p _) = matchedAs classes τ p
matchedAs classes τ (S.PLit ℓ) = meet classes (T.LitTy ℓ) τ
matchedAs classes (T.TupleTy τs) (S.PTuple ps) | length τs == length ps = T.TupleTy (zipWith (matchedAs classes) τs ps)
matchedAs _ τ@(T.ListTy _) (S.PList _) = τ
matchedAs _ τ@(T.DictTy _) (S.PRecord _) = τ
matchedAs classes τ (S.PConstr c _ _) = case instanceAt classes c τ of
   Just (Instance σs) -> meet classes τ (T.ClassTy c σs)
   _ -> never
matchedAs _ _ _ = never

remaining :: ClassTable -> T.Type -> S.Pattern -> T.Type
remaining classes (T.UnionTy σ σ') p = join classes (remaining classes σ p) (remaining classes σ' p)
remaining _ _ (S.PVar _) = never
remaining _ _ S.PWild = never
remaining classes τ (S.PAs p _) = remaining classes τ p
remaining _ τ (S.PLit ℓ)
   | τ == T.LitTy ℓ || (τ == none && ℓ == None) = never
   | τ == bool, Bool b <- ℓ = T.LitTy (Bool (not b))
   | otherwise = τ
remaining classes τ@(T.TupleTy τs) (S.PTuple ps) | length τs == length ps =
   case filter ((_ /= never) <<< snd) (mapWithIndex (×) rests) of
      Nil -> never
      (i × ρ) : Nil -> T.TupleTy (definitely' (updateAt i ρ τs))
      _ -> τ
   where
   rests = zipWith (remaining classes) τs ps
remaining _ (T.DictTy _) (S.PRecord Nil) = never
remaining classes τ (S.PConstr c ps _) = case instanceAt classes c τ of
   Just (Instance σs)
      | subtype classes τ (T.ClassTy c σs)
      , and (zipWith (\(_ × σ) p -> remaining classes σ p == never) (fieldsAt classes c σs) ps) -> never
   _ -> τ
remaining _ τ _ = τ

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

