module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Data.Foldable (foldM)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Eval (GraphConfig, eval_primitives)
import Expr (Stmt, fv)
import File (class LoadFile, File(..), FileCxt(..), fluidExtension, loadFile)

import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import ModuleGraph (DependencyGraph, ModuleName)
import Parse (parseModule, parseProgram)
import SExpr (desugarModuleFwd)
import DefiniteAssignment (class HasClassCtx, ClassCtx, Cxt, Entry(..), TyResult(..), unionWith_mergeEq)
import WellFormed (checkModule, checkProgram, classes, classesOfModule, mainModule)
import SExpr as S
import Util (type (×), error, throwLeft, withMsg, (×))
import Util.Map (constMap, keys, restrict)
import Util.Set ((∪))
import Val (Env)

type Config = { s :: Raw S.Stmt, e :: Raw Stmt, gconfig :: GraphConfig }

builtins :: ModuleName
builtins = "lib/builtins"

prelude :: ModuleName
prelude = "lib/prelude"

-- Demand-driven well-formedness: from the entry's imports, follow the graph and
-- check each reachable module once, under `primitives + its imports' exports`
-- (graph is acyclic, so this terminates). Memoised on each module's exports
-- (own classes + own definitions), which are what importers see.
checkModules
   :: forall m
    . MonadError Error m
   => DependencyGraph
   -> Map ModuleName (Raw S.Module)
   -> Cxt
   -> List ModuleName
   -> m Unit
checkModules graph modules baseCxt roots = void (foldM go Map.empty roots)
   where
   go :: Map ModuleName Cxt -> ModuleName -> m (Map ModuleName Cxt)
   go memo q
      | Map.member q memo = pure memo
      | otherwise = do
           memo' × γ <- foldM step (memo × baseCxt) (fromMaybe Nil (Map.lookup q graph))
           case Map.lookup q modules of
              Nothing -> pure (Map.insert q Map.empty memo')
              Just mod -> do
                 δ <- withMsg ("Checking module " <> q) (checkModule γ mod)
                 λ <- classesOfModule q mod
                 pure (Map.insert q ((Class <$> λ) `Map.union` (Status true <$ δ)) memo')

   step :: Map ModuleName Cxt × Cxt -> ModuleName -> m (Map ModuleName Cxt × Cxt)
   step (memo × acc) i = do
      memo' <- go memo i
      pure (memo' × (acc `Map.union` fromMaybe Map.empty (Map.lookup i memo')))

prepConfig :: forall m. HasClassCtx m => MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Raw Env -> String -> m Config
prepConfig primitives fluidSrc = do
   s × imports <- throwLeft $ parseProgram fluidSrc
   sCxt <- parseModuleGraph (builtins : prelude : imports)
   let moduleClassCtx = Map.insert "__NoArgs" { mod: builtins, base: Nothing, fields: Nil } sCxt.classCtx
   programClasses <- classes mainModule s
   fullClassCtx <- unionWith_mergeEq moduleClassCtx programClasses
   local (\(FileCxt r) -> FileCxt (r { classCtx = fullClassCtx })) do
      modules <- local (\(FileCxt r) -> FileCxt (r { classCtx = moduleClassCtx }))
         $ traverse (\m -> (unit <$ _) <$> desugarModuleFwd (Returns <$ m)) sCxt.modules
      let
         moduleCxt =
            { roots: sCxt.roots
            , topsorted: sCxt.topsorted
            , graph: sCxt.graph
            , modules
            , classCtx: moduleClassCtx
            }
      n × _ × primitives' × _ × topLevelEnv <- flip runAllocT 0 do
         primitives' <- alloc primitives
         modules' <- traverse alloc moduleCxt.modules
         let moduleCxt' = moduleCxt { modules = modules' }
         let mαs = Set.unions (vertices <$> Map.values modules')
         let αs = vertices primitives' ∪ mαs
         _ × γ <- runWithGraphT_spy (eval_primitives primitives' moduleCxt') αs :: AllocT m (GraphImpl × _)
         pure (primitives' × modules' × γ)
      checkModules sCxt.graph sCxt.modules (constMap (Status true) (keys primitives)) (builtins : prelude : imports)
      sty <- checkProgram moduleClassCtx (keys topLevelEnv) s
      eTy <- desug sty
      let e = (unit <$ eTy) :: Raw Stmt
      let gconfig = { n, primitives: primitives', γ: restrict (fv e) topLevelEnv, classCtx: fullClassCtx }
      pure { s, e, gconfig }

-- Desugaring deferred to prepConfig so it runs under a populated ClassCtx.
type SModuleCxt =
   { roots :: List ModuleName
   , topsorted :: List ModuleName
   , graph :: DependencyGraph
   , modules :: Map ModuleName (Raw S.Module)
   , classCtx :: ClassCtx
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
   pure $ { roots, topsorted: topsort graph, graph, modules, classCtx }

   where

   collectModules
      :: Set ModuleName
      -> DependencyGraph
      -> Map ModuleName (Raw S.Module)
      -> ClassCtx
      -> List ModuleName
      -> m (DependencyGraph × Map ModuleName (Raw S.Module) × ClassCtx)
   collectModules visited graph modules classCtx imports = case imports of
      Nil -> pure $ (graph × modules × classCtx)
      mod : rest ->
         if Set.member mod visited then
            collectModules visited graph modules classCtx rest
         else do
            mod' × λ × imports' <- parseAndCollect mod
            classCtx' <- unionWith_mergeEq classCtx λ
            collectModules
               (Set.insert mod visited)
               (Map.insert mod imports' graph)
               (Map.insert mod mod' modules)
               classCtx'
               (imports' <> rest)

   parseAndCollect :: ModuleName -> m (Raw S.Module × ClassCtx × List ModuleName)
   parseAndCollect path = do
      FileCxt { fluidSrcPaths } <- ask
      src <- loadFile fluidSrcPaths (File (path <> fluidExtension))
      mod × imports <- throwLeft <#> withMsg ("Loading module " <> path) $ parseModule src
      λ <- classesOfModule path mod
      let
         imports' =
            if path == builtins then imports
            else if path == prelude then builtins : imports
            else builtins : prelude : imports
      pure $ mod × λ × imports'

   topsort :: DependencyGraph -> List ModuleName
   topsort graph = go (List.fromFoldable $ Map.keys graph) Nil
      where
      go :: List ModuleName -> List ModuleName -> List ModuleName
      go Nil result = reverse result
      go remaining result =
         -- should always be resolvable if no cycles
         case List.find resolved remaining of
            Nothing -> error "Modules contain circular imports"
            Just next -> go (List.delete next remaining) (next : result)
         where
         -- no dependencies or dependencies all resolved
         resolved :: ModuleName -> Boolean
         resolved mod = case Map.lookup mod graph of
            Nothing -> true
            Just deps -> List.all (\dep -> not (List.elem dep remaining)) deps
