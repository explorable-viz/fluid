module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask)
import Control.Monad.State (runStateT)
import Bind (dottedName, pathName, prefixOf, varName)
import Data.List.NonEmpty (snoc, unsnoc)
import Data.List.NonEmpty (fromList) as NEL
import Data.Either (Either(..))
import Data.Foldable (foldM, for_, intercalate)
import Data.List (List(..), catMaybes, elem, filter, reverse, takeWhile, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Classes (class HasClasses, ClassTable)
import Dict (Dict)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Eval (evalImport, implicitMembers, load) as Dep
import Expr (Import(..)) as E
import Literal (Literal(..))
import Expr (Module, Pattern(..), Stmt, fv)
import File (class LoadFile, File(..), FileCxt(..), hasDirectory, loadModuleSource, withClasses)
import DepGraph (Deriv, deriv, emptyGraph)
import Module.Defs (ModuleName, implicit, implicitFor)
import Parse (parseModule, parseProgram)
import Primitive.Defs (predefined, predefinedClasses)
import WellFormed (CheckM, checkProgram, checkModule, mainModule, runCheckM)
import SExpr as S
import Types (Primitive(..)) as T
import Util (MayFail, type (×), orThrow, throw, throwLeft, whenever, withMsg, (×))
import Util.Map (constMap, findWithDefault, mapWithKey, restrict)
import Val (class HasModuleStore, ModuleState(..), modifyModuleStore, Env(..))

type DependencyGraph = Map ModuleName (List ModuleName)

type Config = { s :: S.Stmt, e :: Stmt, inputs :: Dict Deriv, classes :: ClassTable }

isModule :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => ModuleName -> m Boolean
isModule q = do
   FileCxt { fluidSrcPaths } <- ask
   loadModuleSource fluidSrcPaths (pathName q) >>= case _ of
      Just _ -> pure true
      Nothing -> hasDirectory fluidSrcPaths (File (pathName q))

parents :: ModuleName -> List ModuleName
parents q = case NEL.fromList (unsnoc q).init of
   Nothing -> Nil
   Just q' -> parents q' <> (q' : Nil)

importDeps
   :: forall m
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => ModuleName
   -> S.Import
   -> m { edges :: List ModuleName, load :: List ModuleName }
importDeps enclosing (S.Import q f) = do
   ps <- keepModules (parents q)
   subs <- case f of
      Nothing -> pure Nil
      Just xs -> keepModules ((snoc q) <$> xs)
   let
      prefixEdges = case f of
         Nothing -> filter (_ /= enclosing) (parents q)
         Just _ -> filter (\p -> not (p `prefixOf` enclosing)) (parents q)
   pure { edges: (q : subs) <> prefixEdges, load: ps <> (q : subs) }
   where
   keepModules = map catMaybes <<< traverse (\m' -> isModule m' <#> \b -> whenever b m')

checkAcyclic :: DependencyGraph -> List ModuleName -> MayFail Unit
checkAcyclic edges roots = void (foldM (go Nil) Set.empty roots)
   where
   go :: List ModuleName -> Set ModuleName -> ModuleName -> MayFail (Set ModuleName)
   go path done q
      | Set.member q done = pure done
      | q `elem` path = Left
           ("import cycle: " <> intercalate " -> " (dottedName <$> (q : reverse (takeWhile (_ /= q) path)) <> (q : Nil)))
      | otherwise = Set.insert q <$> foldM (go (q : path)) done (findWithDefault Nil q edges)

-- Load modules into a new dependence graph, kept in the store; return the top-level environment as a vertex per variable.
loadTopLevel
   :: forall m
    . HasClasses m
   => HasModuleStore m
   => MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => Map ModuleName Module
   -> List S.Import
   -> m (Dict Deriv)
loadTopLevel modules imports = do
   inputs × depGraph <- flip runStateT emptyGraph do
      predefined' <- traverse (\(_ × Env ρ) -> Loaded <$> traverse deriv ρ) predefined
      modifyModuleStore (_ { modules = Map.union predefined' (Parsed <$> modules) })
      for_ implicit Dep.load
      ρ0 <- Dep.implicitMembers
      foldM (\ρ (S.Import q f) -> Dep.evalImport mainModule ρ (E.Import q f)) ρ0 imports
   modifyModuleStore (_ { depGraph = depGraph })
   pure inputs

prepConfig
   :: forall m
    . HasClasses m
   => HasModuleStore m
   => MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => String
   -> m Config
prepConfig fluidSrc = do
   s × imports <- throwLeft $ parseProgram fluidSrc
   { result: e, modules: mods, classes } <- prepModules imports (checkProgram imports (named mainModule s))
   withClasses classes do
      inputs <- loadTopLevel mods imports
      pure { s, e, inputs: restrict (fv e) inputs, classes }

-- Statement preceded by declaration of module name
named :: ModuleName -> S.Stmt -> S.Stmt
named q = S.Seq (S.Def (S.VarDef (PVar varName) (Just (S.PrimitiveTy T.Str)) (S.Lit (Str (dottedName q)))))

-- Parse modules reachable through imports and run checking action over them, yielding its result, modules
-- checked, and class table.
prepModules
   :: forall m a
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => List S.Import
   -> CheckM a
   -> m { result :: a, modules :: Map ModuleName Module, classes :: ClassTable }
prepModules imports action = do
   mods <- parseModules imports
   result × { modules: checked, classes } <- orThrow (runCheckM action mods (fst <$> predefined) predefinedClasses)
   pure { result, modules: Map.mapMaybe _.mod checked, classes }

-- Module q checked as module, not program, with modules it imports.
prepModule
   :: forall m
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => ModuleName
   -> m { modules :: Map ModuleName Module, classes :: ClassTable }
prepModule q = prepModules (S.Import q Nothing : Nil) (checkModule q) <#> \{ modules, classes } -> { modules, classes }

parseModules
   :: forall m
    . MonadAff m
   => MonadError Error m
   => MonadReader FileCxt m
   => LoadFile m
   => List S.Import
   -> m (Map ModuleName S.Module)
parseModules imports = do
   deps <- traverse (importDeps mainModule) imports
   let roots = implicit <> (deps >>= _.load)
   moduleDeps × mods <- collectModules Set.empty Map.empty Map.empty roots
   orThrow (checkAcyclic moduleDeps (deps >>= _.edges))
   -- prefix-closed: a package with no source file of its own is an empty module
   let ancestors = Set.fromFoldable ((Set.toUnfoldable (Map.keys mods) :: List ModuleName) >>= parents)
   pure (mapWithKey (\q (S.Module ιs s) -> S.Module ιs (named q s)) (mods `Map.union` constMap (S.Module Nil S.Pass) ancestors))

   where

   collectModules
      :: Set ModuleName
      -> DependencyGraph
      -> Map ModuleName S.Module
      -> List ModuleName
      -> m (DependencyGraph × Map ModuleName S.Module)
   collectModules visited moduleDeps mods pending = case pending of
      Nil -> pure $ (moduleDeps × mods)
      mod : rest
         | Set.member mod visited -> collectModules visited moduleDeps mods rest
         | Map.member mod predefined -> do
              shadowed <- isModule mod
              when shadowed $ throw $ "Predefined module cannot have a source file: " <> dottedName mod
              collectModules (Set.insert mod visited) moduleDeps mods rest
         | otherwise -> do
              mod' × edges × toLoad <- parseAndCollect mod
              collectModules
                 (Set.insert mod visited)
                 (Map.insert mod edges moduleDeps)
                 (Map.insert mod mod' mods)
                 (toLoad <> rest)

   parseAndCollect :: ModuleName -> m (S.Module × List ModuleName × List ModuleName)
   parseAndCollect path = do
      FileCxt { fluidSrcPaths } <- ask
      loadModuleSource fluidSrcPaths (pathName path) >>= case _ of
         Just src -> do
            mod × _ <- throwLeft <#> withMsg ("Loading module " <> dottedName path) $ parseModule src
            deps <- case mod of S.Module ιs _ -> traverse (importDeps path) ιs
            let edges = deps >>= _.edges
            let toLoad = implicitFor path <> (deps >>= _.load)
            pure $ mod × edges × toLoad
         Nothing -> hasDirectory fluidSrcPaths (File (pathName path)) >>= case _ of
            true -> pure (S.Module Nil S.Pass × Nil × Nil)
            false -> throw ("Module not found in any path: " <> dottedName path)
