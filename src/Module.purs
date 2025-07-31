module Module where

import Prelude

import Bind (Bind, (↦))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask)
import Data.Bifunctor (lmap)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, Expr, fv)
import File (class LoadFile, File(..), FileCxt(..), Folder, loadFile)
import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, alloc_check, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import ModuleGraph (DependencyGraph, ModuleName, Modules, DependencyGraph')
import Parse (asModule, standalone)
import Parse as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (Module, desugarModuleFwd)
import SExpr as S
import Test.Util.Debug (checking)
import Util (type (×), AffError, concatM, debug, error, (×))
import Util.Map (restrict)
import Util.Parse (SParser)
import Util.Set ((∪))

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< lmap (E.error <<< show) <<< runParser src

parseProgram :: forall m. LoadFile m => Array Folder -> File -> AffError m (Array String × Raw S.Expr)
parseProgram folders (File file) =
   loadFile folders (File (file <> fluidExtension)) >>= flip parse P.program

parseFluidSrc :: forall m. String -> AffError m (Array String × Raw S.Expr)
parseFluidSrc fluidSrc = flip parse P.program fluidSrc

module_ :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ folders (File file) (ProgCxt r@{ mods }) = do
   when debug.logging $ log ("module_: " <> show (folders × file))
   src <- loadFile folders (File (file <> fluidExtension))
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

datasetAs :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs folders (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram folders file >>= desug
   pure $ ProgCxt r { datasets = (x ↦ eα) : datasets }

loadProgCxt :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt datasets = do
   FileCxt { fluidSrcPaths } <- ask
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil })
      >>= concatM (second File >>> datasetAs fluidSrcPaths <$> datasets)

-- updates a progCxt with imported modules
loadMods :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array String -> Raw ProgCxt -> m (Raw ProgCxt)
loadMods mods progCxt = do
   FileCxt { fluidSrcPaths } <- ask
   concatM (File >>> module_ fluidSrcPaths <$> [ "lib/prelude" ] <> mods) progCxt

initialConfig :: forall m a. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig e progCxt = do
   when checking.allocRoundTrip $ alloc_check "progCxt" (alloc progCxt)
   n × _ × progCxt' × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      let αs = vertices progCxt'
      _ × γ <- runWithGraphT_spy (eval_progCxt progCxt') αs :: AllocT m (GraphImpl × _)
      pure (progCxt' × restrict (fv e) γ)
   pure { n, progCxt: progCxt', γ }

initialConfigWithGraph
   :: forall m a
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => FV a
   => a
   -> Raw ProgCxt
   -> Raw DependencyGraph'
   -> List String
   -> m GraphConfig
initialConfigWithGraph e progCxt (sorted × deps × modules) imports = do
   n × _ × progCxt' × _ × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      modules' <- traverse alloc modules
      let graph' = sorted × deps × modules'
      let mαs = Set.unions (vertices <$> Map.values modules')
      let αs = vertices progCxt' ∪ mαs
      _ × γ <- runWithGraphT_spy (eval_progCxt' progCxt' graph' imports) αs :: AllocT m (GraphImpl × _)
      pure (progCxt' × modules' × restrict (fv e) γ)
   pure { n, progCxt: progCxt', γ }

type Config = { s :: Raw S.Expr, e :: Raw Expr, gconfig :: GraphConfig }

prepConfig :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Raw ProgCxt -> String -> m Config
prepConfig progCxt fluidSrc = do
   mods × s <- parseFluidSrc fluidSrc
   e <- desug s
   let imports' = "lib/prelude" : List.fromFoldable imports
   graph <- loadModuleGraph imports'
   gconfig <- initialConfigWithGraph e progCxt graph imports'
   pure { s, e, gconfig }

loadModuleGraph
   :: forall m
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => List ModuleName
   -> m (Raw DependencyGraph')
loadModuleGraph mods = do
   graph × defs <- collectModules Set.empty Map.empty Map.empty mods
   let sorted = topsort graph
   pure (sorted × graph × defs)

   where

   collectModules :: Set ModuleName -> DependencyGraph -> Raw Modules -> List ModuleName -> m (DependencyGraph × Raw Modules)
   collectModules visited graph modules roots = case roots of
      Nil -> pure $ (graph × modules)
      mod : rest ->
         if Set.member mod visited then
            collectModules visited graph modules rest
         else do
            imports × defs <- loadModule mod
            collectModules
               (Set.insert mod visited)
               (Map.insert mod imports graph)
               (Map.insert mod defs modules)
               (imports <> rest)

   loadModule :: ModuleName -> m (List ModuleName × Raw ModuleDefs)
   loadModule name = do
      FileCxt { fluidSrcPaths } <- ask
      src <- loadFile fluidSrcPaths (File name)
      { imports, content } <- parse src (asModule (File name) P.module_)
      mod' <- desugarModuleFwd content
      let imports' = if name == "lib/prelude" then List.fromFoldable imports else "lib/prelude" : List.fromFoldable imports
      pure $ imports' × mod'

   topsort :: DependencyGraph -> List ModuleName
   topsort graph = go (List.fromFoldable $ Map.keys graph) Nil
      where
      go :: List ModuleName -> List ModuleName -> List ModuleName
      go Nil result = reverse result
      go remaining result =
         -- should always be resolvable if no cycles
         case List.find resolved remaining of
            Nothing -> error "cycle!!!"
            Just next -> go (List.delete next remaining) (next : result)
         where
         -- no dependencies or dependencies all resolved
         resolved :: ModuleName -> Boolean
         resolved mod = case Map.lookup mod graph of
            Nothing -> true
            Just deps -> List.all (\dep -> not (List.elem dep remaining)) deps
