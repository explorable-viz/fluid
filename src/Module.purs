module Module where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask)
import Data.Bifunctor (lmap)
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
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, Expr, Module, fv)
import File (class LoadFile, File(..), FileCxt(..), fluidExtension, loadFile)
import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import ModuleGraph (DependencyGraph, ModuleCxt, Modules, ModuleName)
import Parse as P
import Parsing (runParser)
import Pretty (prettyP)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (desugarModuleFwd)
import SExpr as S
import Temp.Parse (parsePy')
import Temp.Util.UnsafeDebug (logUnsafe)
import Util (type (×), AffError, error, (×))
import Util.Map (restrict)
import Util.Parse (SParser)
import Util.Set ((∪))

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< lmap (E.error <<< show) <<< runParser src

parseProgram :: forall m. String -> AffError m (Raw S.Expr × List ModuleName)
parseProgram fluidSrc = flip parse P.program fluidSrc

parseProgram' :: forall m. MonadError Error m => String -> m (Raw S.Expr × List ModuleName)
parseProgram' src = liftEither <<< lmap (E.error <<< show) $ parsePy' src

loadProgCxt :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => m (Raw ProgCxt)
loadProgCxt = pure (ProgCxt { primitives, mods: Nil })

initialConfig
   :: forall m a
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => FV a
   => a
   -> Raw ProgCxt
   -> Raw ModuleCxt
   -> m GraphConfig
initialConfig e progCxt moduleCxt = do
   n × _ × progCxt' × _ × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      modules' <- traverse alloc (moduleCxt.modules)
      let moduleCxt' = moduleCxt { modules = modules' }
      let mαs = Set.unions (vertices <$> Map.values modules')
      let αs = vertices progCxt' ∪ mαs
      _ × γ <- runWithGraphT_spy (eval_progCxt progCxt' moduleCxt') αs :: AllocT m (GraphImpl × _)
      pure (progCxt' × modules' × restrict (fv e) γ)
   pure { n, progCxt: progCxt', γ }

type Config = { s :: Raw S.Expr, e :: Raw Expr, gconfig :: GraphConfig }

prelude :: ModuleName
prelude = "lib/prelude"

prepConfig :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Raw ProgCxt -> String -> m Config
prepConfig progCxt fluidSrc = do
   s × imports <- parseProgram' fluidSrc

   let _ = logUnsafe (prettyP s)

   moduleCxt <- loadModuleGraph (prelude : imports)
   e <- desug s
   gconfig <- initialConfig e progCxt moduleCxt
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
   graph × modules <- collectModules Set.empty Map.empty Map.empty roots
   pure $ { roots, topsorted: topsort graph, graph, modules }

   where

   collectModules :: Set ModuleName -> DependencyGraph -> Raw Modules -> List ModuleName -> m (DependencyGraph × Raw Modules)
   collectModules visited graph modules imports = case imports of
      Nil -> pure $ (graph × modules)
      mod : rest ->
         if Set.member mod visited then
            collectModules visited graph modules rest
         else do
            mod' × imports' <- loadModule mod
            collectModules
               (Set.insert mod visited)
               (Map.insert mod imports' graph)
               (Map.insert mod mod' modules)
               (imports' <> rest)

   loadModule :: ModuleName -> m (Raw Module × List ModuleName)
   loadModule path = do
      FileCxt { fluidSrcPaths } <- ask
      src <- loadFile fluidSrcPaths (File (path <> fluidExtension))
      mod × imports <- parse src P.module_
      mod' <- desugarModuleFwd mod
      let imports' = if path == prelude then imports else prelude : imports
      pure $ mod' × imports'

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
