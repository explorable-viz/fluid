module Module where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Bind (Var, dottedName, pathName)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty (snoc) as NEL
import Data.NonEmpty ((:|))
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Foldable (foldM, foldl)
import Data.List (List(..), takeWhile, (:))
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
import DefiniteAssignment (class HasCxt, ClassEntry, Ctx, Cxt, Entry(..), TyResult(..), unionWith_mergeEq)
import WellFormed (checkModule, checkProgram, classes, classesOfModule, mainModule)
import SExpr as S
import Util (type (×), throw, throwLeft, withMsg, (×))
import Util.Map (constMap, keys, findWithDefault, restrict)
import Util.Set ((∪))
import Val (class HasModuleStore, modifyStore, Env)

type Config = { s :: Raw S.Stmt, e :: Raw Stmt, gconfig :: GraphConfig }

builtins :: ModuleName
builtins = NonEmptyList ("lib" :| "builtins" : Nil)

prelude :: ModuleName
prelude = NonEmptyList ("lib" :| "prelude" : Nil)

predefined :: List ModuleName
predefined = builtins : prelude : Nil

predefinedDeps :: ModuleName -> List ModuleName
predefinedDeps q = takeWhile (_ /= q) predefined

-- The runtime class context is keyed by fully-qualified name (defining module
-- then class), matching the FQNs the desugar bakes into constructors.
fqnKeyed :: Map Var ClassEntry -> Cxt
fqnKeyed m = Map.fromFoldable (reKey <$> (Map.toUnfoldable m :: List _))
   where
   reKey (name × ce) = dottedName (NEL.snoc ce.mod name) × Class ce

type CheckedModules = Map ModuleName Cxt × Map ModuleName (S.Module (TyResult Ctx))

checkModules
   :: DependencyGraph
   -> Map ModuleName (Raw S.Module)
   -> Cxt
   -> List ModuleName
   -> Either String CheckedModules
checkModules graph modules baseCxt roots = foldM go (Map.empty × Map.empty) roots
   where
   go :: CheckedModules -> ModuleName -> Either String CheckedModules
   go acc@(memo × _) q
      | Map.member q memo = pure acc
      | otherwise = do
           memo' × qmods' <- foldM go acc (findWithDefault Nil q graph)
           case Map.lookup q modules of
              Nothing -> pure (Map.insert q Map.empty memo' × qmods')
              Just mod -> do
                 let γ = foldl (\acc' i -> acc' `Map.union` findWithDefault Map.empty i memo') baseCxt (predefinedDeps q)
                 δ × qmod <- lmap (_ <> "\nChecking module " <> dottedName q) (checkModule q memo' γ mod)
                 λ <- classesOfModule q mod
                 pure (Map.insert q ((Class <$> λ) `Map.union` (VarStatus true <$ δ)) memo' × Map.insert q qmod qmods')

prepConfig :: forall m. HasCxt m => HasModuleStore m => MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Raw Env -> String -> m Config
prepConfig primitives fluidSrc = do
   s × imports <- throwLeft $ parseProgram fluidSrc
   sCxt <- parseModuleGraph (predefined <> imports)
   let moduleClassCtx = Map.insert "__NoArgs" { cxt: Map.empty, mod: builtins, base: Nothing, fields: Nil } sCxt.classCtx
   programClasses <- either throw pure (classes mainModule s)
   fullClassCtx <- either throw pure (unionWith_mergeEq moduleClassCtx programClasses)
   memo × qualModules <- either throw pure
      (checkModules sCxt.graph sCxt.modules (constMap (VarStatus true) (keys primitives)) (predefined <> imports))
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
                    foldM importInto primitives' (predefined <> leadingImports s)
               )
               (vertices primitives' ∪ mαs) :: AllocT m (GraphImpl × _)
         pure (primitives' × γ)
      let
         baseCxt =
            foldl (\acc q -> acc `Map.union` findWithDefault Map.empty q memo)
               (constMap (VarStatus true) (keys primitives))
               predefined
               `Map.union` Map.singleton "__NoArgs" (Class { cxt: Map.empty, mod: builtins, base: Nothing, fields: Nil })
      sty <- either throw pure (checkProgram memo baseCxt s)
      eTy <- desug sty
      let e = dropLeadingImports ((unit <$ eTy) :: Raw Stmt)
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
      mod × imports <- throwLeft <#> withMsg ("Loading module " <> dottedName path) $ parseModule src
      λ <- either throw pure (classesOfModule path mod)
      pure $ mod × λ × (predefinedDeps path <> imports)
