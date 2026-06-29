module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Data.Foldable (foldM, foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Eval (GraphConfig, importInto)
import Expr (Stmt, dropLeadingImports, fv)
import File (class LoadFile, File(..), FileCxt(..), fluidExtension, loadFile)

import Graph (vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import ModuleGraph (DependencyGraph, ModuleName)
import Parse (leadingImports, parseModule, parseProgram)
import SExpr (desugarModuleFwd)
import DefiniteAssignment (class HasClassCtx, ClassCtx, Cxt, Entry(..), TyResult(..), unionWith_mergeEq)
import WellFormed (checkModule, checkProgram, classes, classesOfModule, mainModule)
import SExpr as S
import Util (type (×), throwLeft, withMsg, (×))
import Util.Map (constMap, keys, findWithDefault, restrict)
import Util.Set ((∪))
import Val (class HasModuleStore, modifyStore, Env)

type Config = { s :: Raw S.Stmt, e :: Raw Stmt, gconfig :: GraphConfig }

builtins :: ModuleName
builtins = "lib/builtins"

prelude :: ModuleName
prelude = "lib/prelude"

-- Memoised (unlike the spec) on each module's exports so each reachable module
-- is checked at most once; the graph is acyclic, so this terminates.
checkModules
   :: forall m
    . MonadError Error m
   => DependencyGraph
   -> Map ModuleName (Raw S.Module)
   -> Cxt
   -> List ModuleName
   -> m (Map ModuleName Cxt)
checkModules graph modules baseCxt roots = foldM go Map.empty roots
   where
   go :: Map ModuleName Cxt -> ModuleName -> m (Map ModuleName Cxt)
   go memo q
      | Map.member q memo = pure memo
      | otherwise = do
           memo' <- foldM go memo (findWithDefault Nil q graph)
           case Map.lookup q modules of
              Nothing -> pure (Map.insert q Map.empty memo')
              Just mod -> do
                 let γ = foldl (\acc i -> acc `Map.union` findWithDefault Map.empty i memo') baseCxt (predefinedImports q)
                 δ <- withMsg ("Checking module " <> q) (checkModule memo' γ mod)
                 λ <- classesOfModule q mod
                 pure (Map.insert q ((Class <$> λ) `Map.union` (Status true <$ δ)) memo')

   predefinedImports :: ModuleName -> List ModuleName
   predefinedImports q
      | q == builtins = Nil
      | q == prelude = builtins : Nil
      | otherwise = builtins : prelude : Nil

prepConfig :: forall m. HasClassCtx m => HasModuleStore m => MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Raw Env -> String -> m Config
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
                    foldM importInto primitives' (builtins : prelude : leadingImports s)
               )
               (vertices primitives' ∪ mαs) :: AllocT m (GraphImpl × _)
         pure (primitives' × γ)
      memo <- checkModules sCxt.graph sCxt.modules (constMap (Status true) (keys primitives)) (builtins : prelude : imports)
      let
         baseCxt =
            constMap (Status true) (keys primitives)
               `Map.union` findWithDefault Map.empty builtins memo
               `Map.union` findWithDefault Map.empty prelude memo
               `Map.union` Map.singleton "__NoArgs" (Class { mod: builtins, base: Nothing, fields: Nil })
      sty <- checkProgram memo baseCxt s
      eTy <- desug sty
      let e = dropLeadingImports ((unit <$ eTy) :: Raw Stmt)
      let gconfig = { n, primitives: primitives', γ: restrict (fv e) topLevelEnv, classCtx: fullClassCtx }
      pure { s, e, gconfig }

-- Desugaring deferred to prepConfig so it runs under a populated ClassCtx.
type SModuleCxt =
   { roots :: List ModuleName
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
   pure $ { roots, graph, modules, classCtx }

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
