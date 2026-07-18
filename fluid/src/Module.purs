module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Bind (Var, dottedName, pathName)
import Data.List.NonEmpty (snoc, unsnoc, fromList) as NEL
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldM, foldl, intercalate)
import Data.List (List(..), catMaybes, mapMaybe, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Eval (GraphConfig, evalImport, importInto)
import Expr (Import(..)) as E
import Expr (Stmt, fv)
import File (class LoadFile, File(..), FileCxt(..), fluidExtension, loadFile, loadFileMaybe)

import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import ModuleGraph (DependencyGraph, ModuleName, builtins, predefined, predefinedDeps)
import Parse (parseModule, parseProgram)
import SExpr (desugarModuleFwd)
import DefiniteAssignment (class HasCxt, ClassEntry, Ctx, Cxt, Entry(..), TyResult(..), unionWith_mergeEq)
import WellFormed (checkImports, checkModule, checkProgram, classes, classesOfModule, mainModule)
import SExpr as S
import Util (type (×), throw, throwLeft, whenever, withMsg, (×), (∩))
import Util.Map (constMap, keys, findWithDefault, restrict)
import Util.Set ((∪))
import Val (class HasModuleStore, extendEnv, modifyStore, Env)

type Config = { s :: Raw S.Stmt, e :: Raw Stmt, gconfig :: GraphConfig }

-- The runtime class context is keyed by fully-qualified name (defining module
-- then class), matching the FQNs the desugar bakes into constructors.
fqnKeyed :: Map Var ClassEntry -> Cxt
fqnKeyed m = Map.fromFoldable (reKey <$> (Map.toUnfoldable m :: List _))
   where
   reKey (name × ce) = dottedName (NEL.snoc ce.mod name) × Class ce

probeModule :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => ModuleName -> m Boolean
probeModule q = do
   FileCxt { fluidSrcPaths } <- ask
   isJust <$> loadFileMaybe fluidSrcPaths (File (pathName q <> fluidExtension))

parents :: ModuleName -> List ModuleName
parents q = case NEL.fromList (NEL.unsnoc q).init of
   Nothing -> Nil
   Just q' -> parents q' <> (q' : Nil)

importDeps :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => S.Import -> m (List ModuleName)
importDeps (S.Import q f) = do
   ps <- probeAll (parents q)
   subs <- case f of
      Nothing -> pure Nil
      Just xs -> probeAll ((NEL.snoc q) <$> xs)
   pure (ps <> (q : Nil) <> subs)
   where
   probeAll = map catMaybes <<< traverse (\m' -> probeModule m' <#> \b -> whenever b m')

submodules :: Set ModuleName -> ModuleName -> Cxt
submodules known q = Map.fromFoldable (mapMaybe sub (Set.toUnfoldable known))
   where
   sub m = let { init, last: x } = NEL.unsnoc m in whenever (NEL.fromList init == Just q) (x × Mod m)

type CheckedModules = Map ModuleName Cxt × Map ModuleName (S.Module (TyResult Ctx))

checkModules
   :: DependencyGraph
   -> Map ModuleName (Raw S.Module)
   -> Cxt
   -> List ModuleName
   -> Either String CheckedModules
checkModules graph modules baseCxt roots = foldM (go Set.empty) (Map.empty × Map.empty) roots
   where
   go :: Set ModuleName -> CheckedModules -> ModuleName -> Either String CheckedModules
   go visiting acc@(modCxt × _) q
      | Map.member q modCxt || Set.member q visiting = pure acc
      | otherwise = do
           modCxt' × qmods' <- foldM (go (Set.insert q visiting)) acc (findWithDefault Nil q graph)
           case Map.lookup q modules of
              Nothing -> pure (Map.insert q Map.empty modCxt' × qmods')
              Just mod@(S.Module imports _) -> do
                 let γ = foldl (\acc' i -> acc' `Map.union` findWithDefault Map.empty i modCxt') baseCxt (predefinedDeps q)
                 δ × qmod <- lmap (_ <> "\nChecking module " <> dottedName q) (checkModule q modCxt' γ mod)
                 λ <- classesOfModule q mod
                 γImp <- checkImports modCxt' imports
                 let subs = submodules (Map.keys modules) q
                 let clash = (Map.keys γImp ∪ Map.keys δ ∪ Map.keys λ) ∩ Map.keys subs
                 when (not Set.isEmpty clash)
                    $ Left
                    $ "Submodule name clash in module " <> dottedName q <> ": " <> intercalate ", " (Set.toUnfoldable clash :: List Var)
                 let bindings = subs `Map.union` (Class <$> λ) `Map.union` (VarStatus <$> δ)
                 pure (Map.insert q bindings modCxt' × Map.insert q qmod qmods')

prepConfig :: forall m. HasCxt m => HasModuleStore m => MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Raw Env -> String -> m Config
prepConfig primitives fluidSrc = do
   s × imports <- throwLeft $ parseProgram fluidSrc
   importNames <- join <$> traverse importDeps imports
   sCxt <- parseModuleGraph (predefined <> importNames)
   let moduleClassCtx = Map.insert "__NoArgs" { cxt: Map.empty, mod: builtins, base: Nothing, fields: Nil } sCxt.classCtx
   programClasses <- either throw pure (classes mainModule s)
   fullClassCtx <- either throw pure (unionWith_mergeEq moduleClassCtx programClasses)
   modCxt × qualModules <- either throw pure
      (checkModules sCxt.graph sCxt.modules (constMap (VarStatus true) (keys primitives)) (predefined <> importNames))
   local (\(FileCxt r) -> FileCxt (r { classCtx = fqnKeyed fullClassCtx })) do
      modules <- local (\(FileCxt r) -> FileCxt (r { classCtx = fqnKeyed moduleClassCtx }))
         $ traverse (\m -> (unit <$ _) <$> desugarModuleFwd (Returns <$ m)) qualModules
      let
         moduleCxt =
            { roots: sCxt.roots
            , graph: sCxt.graph
            , modules
            , classCtx: moduleClassCtx
            }
      n × _ × primitives' × topLevelEnv <- flip runAllocT 0 do
         primitives' <- alloc primitives
         modules' <- traverse alloc moduleCxt.modules
         let mαs = Set.unions (vertices <$> Map.values modules')
         _ × γ <-
            runWithGraphT_spy
               ( do
                    modifyStore (\st -> st { primitives = primitives', modules = modules', graph = sCxt.graph })
                    γ0 <- foldM importInto primitives' predefined
                    foldM (\γ (S.Import q f) -> (γ `extendEnv` _) <$> evalImport (E.Import q f)) γ0 imports
               )
               (vertices primitives' ∪ mαs) :: AllocT m (GraphImpl × _)
         pure (primitives' × γ)
      let
         baseCxt =
            foldl (\acc q -> acc `Map.union` findWithDefault Map.empty q modCxt)
               (constMap (VarStatus true) (keys primitives))
               predefined
               `Map.union` Map.singleton "__NoArgs" (Class { cxt: Map.empty, mod: builtins, base: Nothing, fields: Nil })
      sty <- either throw pure (checkProgram modCxt baseCxt imports s)
      eTy <- desug sty
      let e = (unit <$ eTy) :: Raw Stmt
      let gconfig = { n, primitives: primitives', γ: restrict (fv e) topLevelEnv, classCtx: fqnKeyed fullClassCtx }
      pure { s, e, gconfig }

-- Desugaring deferred to prepConfig so it runs under a populated class context.
type SModuleCxt =
   { roots :: List ModuleName
   , graph :: DependencyGraph
   , modules :: Map ModuleName (Raw S.Module)
   , classCtx :: Map Var ClassEntry
   }

parseModuleGraph
   :: forall m
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => List ModuleName
   -> m SModuleCxt
parseModuleGraph roots = do
   graph × modules × classCtx <- collectModules Set.empty Map.empty Map.empty Map.empty roots
   pure $ { roots, graph, modules, classCtx }

   where

   collectModules
      :: Set ModuleName
      -> DependencyGraph
      -> Map ModuleName (Raw S.Module)
      -> Map Var ClassEntry
      -> List ModuleName
      -> m (DependencyGraph × Map ModuleName (Raw S.Module) × Map Var ClassEntry)
   collectModules visited graph modules classCtx imports = case imports of
      Nil -> pure $ (graph × modules × classCtx)
      mod : rest ->
         if Set.member mod visited then
            collectModules visited graph modules classCtx rest
         else do
            mod' × λ × imports' <- parseAndCollect mod
            classCtx' <- either throw pure (unionWith_mergeEq classCtx λ)
            collectModules
               (Set.insert mod visited)
               (Map.insert mod imports' graph)
               (Map.insert mod mod' modules)
               classCtx'
               (imports' <> rest)

   parseAndCollect :: ModuleName -> m (Raw S.Module × Map Var ClassEntry × List ModuleName)
   parseAndCollect path = do
      FileCxt { fluidSrcPaths } <- ask
      src <- loadFile fluidSrcPaths (File (pathName path <> fluidExtension))
      mod × _ <- throwLeft <#> withMsg ("Loading module " <> dottedName path) $ parseModule src
      λ <- either throw pure (classesOfModule path mod)
      deps <- case mod of S.Module is _ -> join <$> traverse importDeps is
      pure $ mod × λ × (predefinedDeps path <> deps)
