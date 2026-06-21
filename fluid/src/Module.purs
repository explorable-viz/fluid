module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Eval (GraphConfig, eval_primitives)
import Expr (Module, Stmt, fv)
import File (class LoadFile, File(..), FileCxt(..), fluidExtension, loadFile)

import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import ModuleGraph (DependencyGraph, ModuleCxt, Modules, ModuleName)
import Parse (parseModule, parseProgram)
import SExpr (desugarModuleFwd)
import DefiniteAssignment (class HasClassCtx, ClassCtx, TyResult(..), unionDisjoint)
import WellFormed (checkModule, checkProgram, classesOfModule)
import SExpr as S
import Util (type (×), error, throwLeft, withMsg, (×))
import Util.Map (keys, restrict)
import Util.Set ((∪))
import Val (Env)

type Config = { s :: Raw S.Stmt, e :: Raw Stmt, gconfig :: GraphConfig }

builtins :: ModuleName
builtins = "lib/builtins"

prelude :: ModuleName
prelude = "lib/prelude"

prepConfig :: forall m. HasClassCtx m => MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Raw Env -> String -> m Config
prepConfig primitives fluidSrc = do
   s × imports <- throwLeft $ parseProgram fluidSrc
   moduleCxt <- loadModuleGraph (builtins : prelude : imports)
   -- Inject loaded Λ so downstream code can resolve ctrs via HasClassCtx.
   local (\(FileCxt r) -> FileCxt (r { classCtx = moduleCxt.classCtx })) do
      n × _ × primitives' × _ × topLevelEnv <- flip runAllocT 0 do
         primitives' <- alloc primitives
         modules' <- traverse alloc (moduleCxt.modules)
         let moduleCxt' = moduleCxt { modules = modules' }
         let mαs = Set.unions (vertices <$> Map.values modules')
         let αs = vertices primitives' ∪ mαs
         _ × γ <- runWithGraphT_spy (eval_primitives primitives' moduleCxt') αs :: AllocT m (GraphImpl × _)
         pure (primitives' × modules' × γ)
      sty <- checkProgram moduleCxt.classCtx (keys topLevelEnv) s
      eTy <- desug sty
      let e = (unit <$ eTy) :: Raw Stmt
      let gconfig = { n, primitives: primitives', γ: restrict (fv e) topLevelEnv }
      pure { s, e, gconfig }

loadModuleGraph
   :: forall m
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => List ModuleName
   -> m (Raw ModuleCxt)
loadModuleGraph roots = do
   graph × modules × classCtx <- collectModules Set.empty Map.empty Map.empty Map.empty roots
   pure $ { roots, topsorted: topsort graph, graph, modules, classCtx }

   where

   collectModules
      :: Set ModuleName
      -> DependencyGraph
      -> Raw Modules
      -> ClassCtx
      -> List ModuleName
      -> m (DependencyGraph × Raw Modules × ClassCtx)
   collectModules visited graph modules classCtx imports = case imports of
      Nil -> pure $ (graph × modules × classCtx)
      mod : rest ->
         if Set.member mod visited then
            collectModules visited graph modules classCtx rest
         else do
            mod' × λ × imports' <- loadModule mod
            classCtx' <- unionDisjoint classCtx λ
            collectModules
               (Set.insert mod visited)
               (Map.insert mod imports' graph)
               (Map.insert mod mod' modules)
               classCtx'
               (imports' <> rest)

   loadModule :: ModuleName -> m (Raw Module × ClassCtx × List ModuleName)
   loadModule path = do
      FileCxt { fluidSrcPaths } <- ask
      src <- loadFile fluidSrcPaths (File (path <> fluidExtension))
      mod × imports <- throwLeft <#> withMsg ("Loading module " <> path) $ parseModule src
      checkModule mod
      λ <- classesOfModule mod
      modTy <- desugarModuleFwd (Returns <$ mod)
      let mod' = (unit <$ modTy) :: Raw Module
      let
         imports' =
            if path == builtins then imports
            else if path == prelude then builtins : imports
            else builtins : prelude : imports
      pure $ mod' × λ × imports'

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
