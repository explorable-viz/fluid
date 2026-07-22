module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Bind (Var, dottedName, pathName, prefixOf)
import Data.List.NonEmpty (snoc, unsnoc, fromList) as NEL
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl, intercalate)
import Data.List (List(..), catMaybes, elem, filter, reverse, takeWhile, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import DataType (class HasClasses, ClassTable, cNoArgs)
import Desugarable (desug)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Eval (GraphConfig, evalImport, importInto)
import Expr (Import(..)) as E
import Expr (Module, Stmt, fv)
import File (class LoadFile, File(..), FileCxt(..), fluidExtension, loadFile, loadFileMaybe)

import Graph (Vertex, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (AllocT, alloc, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import ModuleGraph (DependencyGraph, ModuleName, builtins, predefined, predefinedDeps)
import Parse (parseModule, parseProgram)
import SExpr (desugarModuleFwd)
import DefiniteAssignment (ClassEntry, Cxt, Entry(..), WfResult(..), unionWith_mergeEq)
import WellFormed (checkProgram, classes, mainModule)
import SExpr as S
import Util (type (×), check, orThrow, throwLeft, whenever, withMsg, (×))
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

importDeps :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => ModuleName -> S.Import -> m { edges :: List ModuleName, load :: List ModuleName }
importDeps enclosing (S.Import q f) = do
   ps <- probeAll (parents q)
   subs <- case f of
      Nothing -> pure Nil
      Just xs -> probeAll ((NEL.snoc q) <$> xs)
   let
      prefixEdges = case f of
         Nothing -> filter (_ /= enclosing) (parents q)
         Just _ -> filter (\p -> not (p `prefixOf` enclosing)) (parents q)
   pure { edges: (q : subs) <> prefixEdges, load: ps <> (q : subs) }
   where
   probeAll = map catMaybes <<< traverse (\m' -> probeModule m' <#> \b -> whenever b m')

checkAcyclic :: DependencyGraph -> List ModuleName -> Either String Unit
checkAcyclic edges roots = void (foldM (go Nil) Set.empty roots)
   where
   go :: List ModuleName -> Set ModuleName -> ModuleName -> Either String (Set ModuleName)
   go path done q
      | Set.member q done = pure done
      | q `elem` path = Left ("import cycle: " <> intercalate " -> " (dottedName <$> (q : reverse (takeWhile (_ /= q) path)) <> (q : Nil)))
      | otherwise = Set.insert q <$> foldM (go (q : path)) done (findWithDefault Nil q edges)

noArgsClass :: ClassEntry
noArgsClass = { cxt: Map.empty, mod: builtins, base: Nothing, fields: Nil }

moduleClasses :: Map ModuleName Cxt -> Map Var ClassEntry
moduleClasses modCxt =
   Map.insert (dottedName cNoArgs) noArgsClass (foldl Map.union Map.empty (fqnKeyed <<< classesOf <$> Map.values modCxt))
   where
   classesOf = Map.mapMaybe case _ of
      Class cls -> Just cls
      _ -> Nothing

withClasses :: forall m a. MonadReader FileCxt m => ClassTable -> m a -> m a
withClasses classes = local (\(FileCxt r) -> FileCxt (r { classes = classes }))

allocTopLevel
   :: forall m
    . HasClasses m
   => HasModuleStore m
   => MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => Raw Env
   -> Map ModuleName (Raw Module)
   -> List S.Import
   -> m (Int × Env Vertex)
allocTopLevel primitives modules imports = do
   n × _ × topLevelEnv <- flip runAllocT 0 do
      primitives' <- alloc primitives
      modules' <- traverse alloc modules
      let mαs = Set.unions (vertices <$> Map.values modules')
      _ × γ <-
         runWithGraphT_spy
            ( do
                 modifyStore (\st -> st { primitives = primitives', modules = modules' })
                 γ0 <- foldM importInto primitives' predefined
                 modifyStore (_ { builtinsEnv = γ0 })
                 γ1 <- foldM (\γ (S.Import q f) -> evalImport mainModule γ (E.Import q f)) empty imports
                 vName <- val Nothing Set.empty (V.Str "__main__")
                 pure (γ1 <+> maplet "__name__" vName)
            )
            (vertices primitives' ∪ mαs) :: AllocT m (GraphImpl × _)
      pure γ
   pure (n × topLevelEnv)

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
   let baseCxt = constMap (VarStatus true) (keys primitives) `Map.union` Map.singleton "__NoArgs" (Class noArgsClass)
   modules <- parseModules imports
   { γ: γ_wf, s: s_wf, loaded } <- orThrow (checkProgram modules baseCxt imports s)
   let modClasses = moduleClasses (_.cxt <$> loaded)
   allClasses <- orThrow do
      programClasses <- classes mainModule s
      unionWith_mergeEq modClasses (fqnKeyed programClasses)
   withClasses allClasses do
      coreModules <- withClasses modClasses
         $ traverse (\m -> (unit <$ _) <$> desugarModuleFwd (Returns <$ m)) (_.mod <$> loaded)
      n × topLevelEnv <- allocTopLevel primitives coreModules imports
      check (Map.keys γ_wf == Set.fromFoldable (keys topLevelEnv)) "reduced context matches top-level environment"
      e_wf <- desug s_wf
      let e = (unit <$ e_wf) :: Raw Stmt
      let gconfig = { n, γ: restrict (fv e) topLevelEnv, classes: allClasses }
      pure { s, e, gconfig }

parseModules
   :: forall m
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => List S.Import
   -> m (Map ModuleName (Raw S.Module))
parseModules imports = do
   imported <- traverse (importDeps mainModule) imports
   let roots = predefined <> (imported >>= _.load)
   importGraph × modules <- collectModules Set.empty Map.empty Map.empty roots
   orThrow (checkAcyclic importGraph (imported >>= _.edges))
   -- prefix-closed: a package with no source file of its own is an empty module
   let ancestors = Set.fromFoldable ((Set.toUnfoldable (Map.keys modules) :: List ModuleName) >>= parents)
   pure (modules `Map.union` constMap (S.Module Nil Nil) ancestors)

   where

   collectModules
      :: Set ModuleName
      -> DependencyGraph
      -> Map ModuleName (Raw S.Module)
      -> List ModuleName
      -> m (DependencyGraph × Map ModuleName (Raw S.Module))
   collectModules visited importGraph modules pending = case pending of
      Nil -> pure $ (importGraph × modules)
      mod : rest ->
         if Set.member mod visited then
            collectModules visited importGraph modules rest
         else do
            mod' × edges × toLoad <- parseAndCollect mod
            collectModules
               (Set.insert mod visited)
               (Map.insert mod edges importGraph)
               (Map.insert mod mod' modules)
               (toLoad <> rest)

   parseAndCollect :: ModuleName -> m (Raw S.Module × List ModuleName × List ModuleName)
   parseAndCollect path = do
      FileCxt { fluidSrcPaths } <- ask
      src <- loadFile fluidSrcPaths (File (pathName path <> fluidExtension))
      mod × _ <- throwLeft <#> withMsg ("Loading module " <> dottedName path) $ parseModule src
      imported <- case mod of S.Module is _ -> traverse (importDeps path) is
      let edges = imported >>= _.edges
      let toLoad = predefinedDeps path <> (imported >>= _.load)
      pure $ mod × edges × toLoad
