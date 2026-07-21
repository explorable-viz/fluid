module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Bind (Var, dottedName, pathName, prefixOf)
import Data.List.NonEmpty (snoc, unsnoc, fromList) as NEL
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldM, intercalate)
import Data.List (List(..), catMaybes, elem, filter, mapMaybe, reverse, takeWhile, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DataType (class HasClasses, cNoArgs, classTable)
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
import DefiniteAssignment (ClassEntry, VarCxt, Cxt, Entry(..), WfResult(..), unionWith_mergeEq)
import WellFormed (checkImports, checkModule, checkProgram, classes, classesOfModule, mainModule)
import SExpr as S
import Util (type (×), check, orThrow, throwLeft, whenever, withMsg, (×), (∩))
import Util.Map (constMap, keys, findWithDefault, maplet, restrict, (<+>))
import Util.Set ((∪), empty)
import Val (class HasModuleStore, modifyStore, val, Env)
import Val (BaseVal(..)) as V

type Config = { s :: Raw S.Stmt, e :: Raw Stmt, gconfig :: GraphConfig }

-- The class context is keyed by fully-qualified name (defining module then
-- class), matching the FQNs the desugar bakes into constructors.
fqnKeyed :: Map Var ClassEntry -> Map Var ClassEntry
fqnKeyed m = Map.fromFoldable (reKey <$> (Map.toUnfoldable m :: List _))
   where
   reKey (name × cls) = dottedName (NEL.snoc cls.mod name) × cls

probeModule :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => ModuleName -> m Boolean
probeModule q = do
   FileCxt { fluidSrcPaths } <- ask
   isJust <$> loadFileMaybe fluidSrcPaths (File (pathName q <> fluidExtension))

parents :: ModuleName -> List ModuleName
parents q = case NEL.fromList (NEL.unsnoc q).init of
   Nothing -> Nil
   Just q' -> parents q' <> (q' : Nil)

importDeps :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => ModuleName -> S.Import -> m (List ModuleName × List ModuleName)
importDeps enclosing (S.Import q f) = do
   ps <- probeAll (parents q)
   subs <- case f of
      Nothing -> pure Nil
      Just xs -> probeAll ((NEL.snoc q) <$> xs)
   let
      prefixEdges = case f of
         Nothing -> filter (_ /= enclosing) (parents q)
         Just _ -> filter (\p -> not (p `prefixOf` enclosing)) (parents q)
   pure (((q : subs) <> prefixEdges) × (ps <> (q : subs)))
   where
   probeAll = map catMaybes <<< traverse (\m' -> probeModule m' <#> \b -> whenever b m')

submodules :: Set ModuleName -> ModuleName -> Cxt
submodules known q = Map.fromFoldable (mapMaybe sub (Set.toUnfoldable known))
   where
   sub m = let { init, last: x } = NEL.unsnoc m in whenever (NEL.fromList init == Just q) (x × Mod m)

checkAcyclic :: DependencyGraph -> List ModuleName -> Either String Unit
checkAcyclic edges roots = void (foldM (go Nil) Set.empty roots)
   where
   go :: List ModuleName -> Set ModuleName -> ModuleName -> Either String (Set ModuleName)
   go path done q
      | Set.member q done = pure done
      | q `elem` path = Left ("import cycle: " <> intercalate " -> " (dottedName <$> (q : reverse (takeWhile (_ /= q) path)) <> (q : Nil)))
      | otherwise = Set.insert q <$> foldM (go (q : path)) done (findWithDefault Nil q edges)

type CheckedModules = Map ModuleName Cxt × Map ModuleName (S.Module (WfResult VarCxt))

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
                 δ × qmod <- lmap (_ <> "\nChecking module " <> dottedName q) (checkModule q modCxt' baseCxt mod)
                 λ <- classesOfModule q mod
                 _ × γ_imp <- checkImports q baseCxt modCxt' imports
                 let subs = submodules (Map.keys modules) q
                 let clash = (Map.keys γ_imp ∪ Map.keys δ ∪ Map.keys λ) ∩ Map.keys subs
                 when (not Set.isEmpty clash)
                    $ Left
                    $ "Submodule name clash in module " <> dottedName q <> ": " <> intercalate ", " (Set.toUnfoldable clash :: List Var)
                 let bindings = (if q == builtins then baseCxt else Map.empty) `Map.union` subs `Map.union` (Class <$> λ) `Map.union` (VarStatus <$> δ)
                 pure (Map.insert q bindings modCxt' × Map.insert q qmod qmods')

noArgsClass :: ClassEntry
noArgsClass = { cxt: Map.empty, mod: builtins, base: Nothing, fields: Nil }

prepConfig
   :: forall m
    . HasClasses m
   => HasModuleStore m
   => MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => Raw Env
   -> String
   -> m Config
prepConfig primitives fluidSrc = do
   s × imports <- throwLeft $ parseProgram fluidSrc
   pairs <- traverse (importDeps mainModule) imports
   let importNames = pairs >>= snd
   let roots = predefined <> importNames
   let primCxt = constMap (VarStatus true) (keys primitives)
   sCxt <- parseModuleGraph roots
   moduleClasses × allClasses × modCxt × qmods <- orThrow do
      checkAcyclic sCxt.importGraph (pairs >>= fst)
      let moduleClasses = Map.insert (dottedName cNoArgs) noArgsClass sCxt.classCtx
      programClasses <- classes mainModule s
      allClasses <- unionWith_mergeEq moduleClasses (fqnKeyed programClasses)
      modCxt × qmods <- checkModules sCxt.graph sCxt.modules primCxt roots
      pure (moduleClasses × allClasses × modCxt × qmods)
   let allClassTable = classTable allClasses
   local (\(FileCxt r) -> FileCxt (r { classes = allClassTable })) do
      modules <- local (\(FileCxt r) -> FileCxt (r { classes = classTable moduleClasses }))
         $ traverse (\m -> (unit <$ _) <$> desugarModuleFwd (Returns <$ m)) qmods
      let
         moduleCxt =
            { roots: sCxt.roots
            , graph: sCxt.graph
            , modules
            , classCtx: moduleClasses
            }
      n × _ × topLevelEnv <- flip runAllocT 0 do
         primitives' <- alloc primitives
         modules' <- traverse alloc moduleCxt.modules
         let mαs = Set.unions (vertices <$> Map.values modules')
         _ × γ <-
            runWithGraphT_spy
               ( do
                    modifyStore (\st -> st { primitives = primitives', modules = modules', graph = sCxt.graph })
                    γ0 <- foldM importInto primitives' predefined
                    modifyStore (_ { builtinsEnv = γ0 })
                    γ1 <- foldM (\γ (S.Import q f) -> evalImport mainModule γ (E.Import q f)) empty imports
                    vName <- val Nothing Set.empty (V.Str "__main__")
                    pure (γ1 <+> maplet "__name__" vName)
               )
               (vertices primitives' ∪ mαs) :: AllocT m (GraphImpl × _)
         pure γ
      let baseCxt = primCxt `Map.union` Map.singleton "__NoArgs" (Class noArgsClass)
      γ_wf × s_wf <- orThrow (checkProgram modCxt baseCxt imports s)
      check (Map.keys γ_wf == Set.fromFoldable (keys topLevelEnv)) "reduced context matches top-level environment"
      e_wf <- desug s_wf
      let e = (unit <$ e_wf) :: Raw Stmt
      let gconfig = { n, γ: restrict (fv e) topLevelEnv, classes: allClassTable }
      pure { s, e, gconfig }

-- Desugaring deferred to prepConfig so it runs under a populated class context.
type SModuleCxt =
   { roots :: List ModuleName
   , graph :: DependencyGraph
   , importGraph :: DependencyGraph
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
   graph × importGraph × modules × classCtx <- collectModules Set.empty Map.empty Map.empty Map.empty Map.empty roots
   pure $ { roots, graph, importGraph, modules, classCtx }

   where

   collectModules
      :: Set ModuleName
      -> DependencyGraph
      -> DependencyGraph
      -> Map ModuleName (Raw S.Module)
      -> Map Var ClassEntry
      -> List ModuleName
      -> m (DependencyGraph × DependencyGraph × Map ModuleName (Raw S.Module) × Map Var ClassEntry)
   collectModules visited graph importGraph modules classCtx imports = case imports of
      Nil -> pure $ (graph × importGraph × modules × classCtx)
      mod : rest ->
         if Set.member mod visited then
            collectModules visited graph importGraph modules classCtx rest
         else do
            mod' × λ × edges × deps <- parseAndCollect mod
            classCtx' <- orThrow (unionWith_mergeEq classCtx (fqnKeyed λ))
            collectModules
               (Set.insert mod visited)
               (Map.insert mod deps graph)
               (Map.insert mod edges importGraph)
               (Map.insert mod mod' modules)
               classCtx'
               (deps <> rest)

   parseAndCollect :: ModuleName -> m (Raw S.Module × Map Var ClassEntry × List ModuleName × List ModuleName)
   parseAndCollect path = do
      FileCxt { fluidSrcPaths } <- ask
      src <- loadFile fluidSrcPaths (File (pathName path <> fluidExtension))
      mod × _ <- throwLeft <#> withMsg ("Loading module " <> dottedName path) $ parseModule src
      λ <- orThrow (classesOfModule path mod)
      pairs <- case mod of S.Module is _ -> traverse (importDeps path) is
      let edges = pairs >>= fst
      let deps = predefinedDeps path <> (pairs >>= snd)
      pure $ mod × λ × edges × deps
