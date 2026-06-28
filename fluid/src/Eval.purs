module Eval where

import Prelude hiding (absurd, apply)

import Bind (varAnon)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader, local)
import DefiniteAssignment (class HasClassCtx, ClassCtx, askClassCtx, fields)
import Data.Array ((..))
import Data.List (List(..), find, foldM, length, snoc, unzip, zip, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((***))
import Data.Set (Set, insert)
import Data.Set as Set
import Data.Traversable (class Foldable, for, sequence, traverse)
import Data.Tuple (curry, fst, snd)
import DataType (arity, checkArity, consistentWith, dataType, showCtr)
import Dict (Dict)
import Dict (fromFoldable) as D
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs(..), Stmt(..), VarDef(..), asStmt, fv)
import File (class LoadFile, FileCxt(..))
import GaloisConnection (GaloisConnection(..))
import Graph (class Graph, Vertex, op, selectαs, select𝔹s, showGraph, showVertices, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice)
import Graph.WithGraph (class MonadWithGraphAlloc, alloc, new, runAllocT, runWithGraphT_spy)
import Lattice (Raw, 𝔹)
import ModuleGraph (ModuleName)
import Pretty (prettyP)
import Primitive (intPair, string, unpack)
import Test.Util.Debug (checking, tracing)
import Util (type (×), Endo, absurd, check, definitely, error, orElse, singleton, spyFunWhen, throw, traceWhen, withMsg, (×), (⊆))
import Util.Map (unionWith_never, get, keys, lookup, lookup', maplet, restrict, (<+>))
import Util.Pair (unzip) as P
import Util.Set ((∪), empty)
import Val (BaseVal(..), Fun(..)) as V
import Val (class HasModuleStore, getStore, modifyStore, BaseVal, DictRep(..), Env(..), EnvStmt(..), ForeignOp(..), ForeignOp'(..), MatrixDim(..), MatrixRep(..), Result(..), Val(..), asReturns, forDefs, val)

-- Needs a better name.
type GraphConfig =
   { primitives :: Env Vertex
   , n :: Int
   , γ :: Env Vertex
   , classCtx :: ClassCtx
   }

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall m. HasClassCtx m => MonadWithGraphAlloc m => Val Vertex -> Elim Vertex -> m (Env Vertex × Cont Vertex × Set Vertex)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × empty)
   | otherwise = pure (maplet x v × κ × empty)
match (Val α _ (V.Constr c vs)) (ElimConstr m) = do
   λ <- askClassCtx
   withMsg "Pattern mismatch" $ consistentWith λ (Set.singleton c) (keys m)
   κ <- lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
   γ × κ' × αs <- matchMany vs κ
   pure (γ × κ' × (insert α αs))
match v (ElimConstr m) = do
   λ <- askClassCtx
   d <- case Set.toUnfoldable (keys m) :: List _ of
      c : _ -> maybe (throw $ "Unknown constructor: " <> showCtr c) pure (dataType λ c)
      Nil -> throw "Pattern matched empty ElimConstr"
   throw $ patternMismatch (prettyP v) (show d)
match (Val α _ (V.Dictionary (DictRep xvs))) (ElimDict xs κ) = do
   check (Set.subset xs (Set.fromFoldable $ keys xvs))
      $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # Set.toUnfoldable
   let xvs' = unwrap xvs
   γ × κ' × αs <- matchMany (map (\k -> snd (get k xvs')) xs') κ
   pure $ γ × κ' × (insert α αs)
match v (ElimDict xs _) = throw (patternMismatch (prettyP v) (show xs))

matchMany :: forall m. HasClassCtx m => MonadWithGraphAlloc m => List (Val Vertex) -> Cont Vertex -> m (Env Vertex × Cont Vertex × Set Vertex)
matchMany Nil κ = pure (empty × κ × empty)
matchMany (v : vs) (ContElim σ) = do
   γ × κ × αs <- match v σ
   γ' × κ' × βs <- matchMany vs κ
   pure $ γ `unionWith_never` γ' × κ' × (αs ∪ βs)
matchMany (_ : vs) (ContStmt _) = throw $
   show (length vs + 1) <> " extra argument(s) to constructor/dictionary; did you forget parentheses in lambda pattern?"

closeDefs :: forall m. HasClassCtx m => MonadWithGraphAlloc m => Env Vertex -> Dict (Elim Vertex) -> Set Vertex -> m (Env Vertex)
closeDefs γ ρ αs =
   Env <$> for ρ \σ ->
      let
         ρ' = ρ `forDefs` σ
      in
         val Nothing αs (V.Fun (V.Closure (restrict (fv ρ' ∪ fv σ) γ) ρ' σ))

apply
   :: forall m
    . HasClassCtx m
   => HasModuleStore m
   => MonadWithGraphAlloc m
   => MonadReader FileCxt m
   => MonadAff m
   => LoadFile m
   => Maybe (Val Vertex)
   -> Val Vertex
   -> Val Vertex
   -> m (Val Vertex)
apply doc_opt (Val α _ (V.Fun (V.Closure γ1 ρ σ))) v = do
   γ2 <- closeDefs γ1 ρ (singleton α)
   γ3 × κ × αs <- match v σ
   asReturns <$> evalStmt doc_opt (γ1 <+> γ2 <+> γ3) (asStmt κ) (insert α αs)
apply doc_opt (Val α _ (V.Fun (V.Foreign (ForeignOp (id × φ)) vs))) v =
   apply' φ
   where
   vs' = snoc vs v

   apply' :: ForeignOp' -> m (Val Vertex)
   apply' (ForeignOp' φ') =
      if φ'.arity > length vs' then
         val doc_opt (singleton α) v'
      else
         φ'.op doc_opt vs'
      where
      v' = V.Fun (V.Foreign (ForeignOp (id × φ)) vs')
apply doc_opt (Val α _ (V.Fun (V.PartialConstr c vs))) v = do
   n <- askClassCtx >>= \λ -> maybe (throw $ "Unknown constructor: " <> showCtr c) pure (arity λ c)
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   let
      v' =
         if length vs < n - 1 then
            V.Fun (V.PartialConstr c (snoc vs v))
         else
            V.Constr c (snoc vs v)
   val doc_opt (singleton α) v'
apply _ _ v = throw $ "Found " <> prettyP v <> ", expected function"

eval
   :: forall m
    . HasClassCtx m
   => HasModuleStore m
   => MonadWithGraphAlloc m
   => MonadReader FileCxt m
   => MonadAff m
   => LoadFile m
   => Maybe (Val Vertex) -- optional doc-comment context
   -> Env Vertex
   -> Expr Vertex
   -> Set Vertex
   -> m (Val Vertex)
eval doc_opt γ e0 αs = do
   αu_opt <- evalVal γ e0 αs
   case αu_opt of
      Just (α × u) ->
         new (flip Val doc_opt) (insert α αs) u
      Nothing -> case e0 of
         Var x -> do
            traceWhen (isJust doc_opt) $ "Discarding doc (variable " <> x <> ")"
            withMsg "Variable lookup" $ lookup' x γ
         Op op -> do
            traceWhen (isJust doc_opt) $ "Discarding doc (operator " <> op <> ")"
            withMsg "Variable lookup" $ lookup' op γ
         DProject e e' -> do
            traceWhen (isJust doc_opt) $ "Discarding doc (projection)"
            v <- eval Nothing γ e αs
            v' <- eval Nothing γ e' αs
            case v, v' of
               Val _ _ (V.Dictionary (DictRep d)), Val _ _ (V.Str s) ->
                  withMsg "Dict lookup" $ snd <$> lookup s d # orElse ("Key \"" <> s <> "\" not found")
               Val _ _ (V.Constr c vs), Val _ _ (V.Str x) -> do
                  xs <- askClassCtx >>= flip fields c
                  find (\(k × _) -> k == x) (zip xs vs) <#> snd # orElse (c <> " has no field " <> x)
               Val _ _ (V.Dictionary _), _ -> throw $ "Found " <> prettyP v' <> ", expected string"
               _, _ -> throw $ "Found " <> prettyP v <> ", expected dict or object"
         App e e' -> do
            v <- eval Nothing γ e αs
            v' <- eval Nothing γ e' αs
            withMsg ("In " <> funName e) $ apply doc_opt v v'
         DocExpr e e' -> do
            v <- eval Nothing γ e αs
            traceWhen (isJust doc_opt) "Outer doc trumps inner doc"
            eval (Just $ fromMaybe v doc_opt) γ e' αs
         _ -> error absurd
   where
   funName :: forall a. Expr a -> String
   funName (Var x) = x
   funName (Op op) = op
   funName (App e _) = funName e
   funName _ = "unknown"

evalStmt
   :: forall m
    . HasClassCtx m
   => HasModuleStore m
   => MonadWithGraphAlloc m
   => MonadReader FileCxt m
   => MonadAff m
   => LoadFile m
   => Maybe (Val Vertex)
   -> Env Vertex
   -> Stmt Vertex
   -> Set Vertex
   -> m (Result Vertex)
evalStmt doc_opt γ s αs = case s of
   Return e -> Returns <$> eval doc_opt γ e αs
   Match e σ -> do
      v <- eval Nothing γ e αs
      case σ, v of
         ElimConstr m, Val _ _ (V.Constr c _) | not (isJust (lookup c m)) ->
            pure (Assigns empty empty)
         _, _ -> do
            γ' × κ × αs' <- match v σ
            evalStmt doc_opt (γ <+> γ') (asStmt κ) (αs ∪ αs')
   Def (VarDef σ e) -> do
      v <- eval Nothing γ e αs
      γ' × _ × αs' <- withMsg "In assignment" $ match v σ
      pure (Assigns γ' αs')
   DefRec (RecDefs α ρ) -> do
      γ' <- closeDefs γ ρ (insert α αs)
      pure (Assigns γ' (insert α αs))
   Pass -> pure (Assigns empty empty)
   ExprStmt e -> do
      _ <- eval Nothing γ e αs
      pure (Assigns empty empty)
   Import q -> do
      load q
      { cache } <- getStore
      pure (Assigns (definitely "import loaded" (Map.lookup q cache)) empty)
   Seq s1 s2 -> do
      r1 <- evalStmt Nothing γ s1 αs
      case r1 of
         Returns _ -> pure r1
         Assigns γ' αs' -> evalStmt doc_opt (γ <+> γ') s2 αs'

evalVal
   :: forall m
    . HasClassCtx m
   => HasModuleStore m
   => MonadWithGraphAlloc m
   => MonadReader FileCxt m
   => MonadAff m
   => LoadFile m
   => Env Vertex
   -> Expr Vertex
   -> Set Vertex
   -> m (Maybe (Vertex × BaseVal Vertex))
evalVal _ (Int α n) _ =
   pure $ Just (α × V.Int n)
evalVal _ (Float α n) _ =
   pure $ Just (α × V.Float n)
evalVal _ (Str α s) _ =
   pure $ Just (α × V.Str s)
evalVal γ (Dictionary α ees) αs = do
   vs × us <- traverse (traverse (flip (eval Nothing γ) αs)) ees <#> P.unzip
   let
      ss × βs = (vs <#> unpack string) # unzip
      d = D.fromFoldable $ zip ss (zip βs us)
   pure $ Just (α × V.Dictionary (DictRep d))
evalVal γ (Constr α c es) αs = do
   askClassCtx >>= \λ -> checkArity λ c (length es)
   vs <- traverse (flip (eval Nothing γ) αs) es
   pure $ Just (α × V.Constr c vs)
evalVal γ (Matrix α e (x × y) e') αs = do
   Val _ _ v <- eval Nothing γ e' αs
   let (i' × β) × (j' × β') = intPair.unpack v
   check
      (i' × j' >= 1 × 1)
      ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   vss <- sequence do
      i <- 0 .. (i' - 1)
      singleton $ sequence do
         j <- 0 .. (j' - 1)
         let γ' = maplet x (Val β Nothing (V.Int i)) `unionWith_never` (maplet y (Val β' Nothing (V.Int j)))
         singleton (eval Nothing (γ <+> γ') e αs)
   pure $ Just (α × V.Matrix (MatrixRep (vss × MatrixDim (i' × β) × MatrixDim (j' × β'))))
evalVal γ (Lambda α σ) _ =
   pure $ Just (α × V.Fun (V.Closure (restrict (fv σ) γ) empty σ))
evalVal _ _ _ = pure Nothing

eval_module :: forall m. HasClassCtx m => HasModuleStore m => MonadWithGraphAlloc m => MonadReader FileCxt m => MonadAff m => LoadFile m => Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
eval_module γ = go empty
   where
   go :: Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
   go γ' (Module Nil) _ = pure γ'
   go γ' (Module (s : ss)) αs = do
      γ'' × αs' <- step γ' s αs
      go (γ' <+> γ'') (Module ss) αs'

   step γ' (Def (VarDef σ e)) αs = do
      v <- eval Nothing (γ <+> γ') e αs
      γ'' × _ × αs' <- match v σ
      pure (γ'' × αs')
   step γ' (DefRec (RecDefs α ρ)) αs = do
      γ'' <- closeDefs (γ <+> γ') ρ (insert α αs)
      pure (γ'' × αs)
   step _ _ αs = pure (empty × αs)

importInto :: forall m. HasClassCtx m => HasModuleStore m => MonadWithGraphAlloc m => MonadReader FileCxt m => MonadAff m => LoadFile m => Env Vertex -> ModuleName -> m (Env Vertex)
importInto γ q = do
   load q
   { cache } <- getStore
   pure (γ <+> definitely "module loaded" (Map.lookup q cache))

load :: forall m. HasClassCtx m => HasModuleStore m => MonadWithGraphAlloc m => MonadReader FileCxt m => MonadAff m => LoadFile m => ModuleName -> m Unit
load q = do
   { primitives, modules, graph, cache } <- getStore
   unless (Map.member q cache) do
      γ_q <- foldM importInto primitives (fromMaybe Nil (Map.lookup q graph))
      γ' <- maybe (pure empty) (\defs' -> eval_module γ_q defs' empty) (Map.lookup q modules)
      modifyStore (\s -> s { cache = Map.insert q γ' s.cache })

type GraphEval g s t =
   { g :: g
   , graph_fwd :: Set Vertex -> Endo g
   , graph_bwd :: Set Vertex -> Endo g
   , inα :: s Vertex
   , outα :: t Vertex
   }

withOp :: forall g s t. Graph g => GraphEval g s t -> GraphEval g t s
withOp { g, graph_fwd, graph_bwd, inα, outα } =
   { g: op g, graph_fwd, graph_bwd, inα: outα, outα: inα }

graphGC
   :: forall g s t
    . Graph g
   => Apply s
   => Apply t
   => Foldable s
   => Foldable t
   => GraphEval g s t
   -> { fwd :: s 𝔹 -> t 𝔹 × g
      , bwd :: t 𝔹 -> s 𝔹 × g
      }
graphGC { g, graph_fwd, graph_bwd, inα, outα } =
   { fwd: \in𝔹 ->
        let
           g' = graph_fwd (selectαs in𝔹 inα) g
        in
           select𝔹s outα (vertices g') × g'
   , bwd: \out𝔹 ->
        let
           g' = graph_bwd (selectαs out𝔹 outα) g
        in
           select𝔹s inα (vertices g') × g'
   }

toGC
   :: forall g s t
    . Graph g
   => Apply s
   => Apply t
   => Foldable s
   => Foldable t
   => { fwd :: s 𝔹 -> t 𝔹 × g
      , bwd :: t 𝔹 -> s 𝔹 × g
      }
   -> GaloisConnection (s 𝔹) (t 𝔹)
toGC { fwd, bwd } = GC { fwd: fst <<< fwd, bwd: fst <<< bwd }

graphEval :: forall m. HasClassCtx m => HasModuleStore m => MonadAff m => MonadReader FileCxt m => LoadFile m => MonadError Error m => GraphConfig -> Raw Stmt -> m (GraphEval GraphImpl EnvStmt Val)
graphEval { n, γ, classCtx } stmt =
   local (\(FileCxt r) -> FileCxt (r { classCtx = classCtx })) do
      _ × _ × g × inα × outα <- flip runAllocT n do
         sα <- alloc stmt
         let inα = EnvStmt γ sα
         g × outα <- runWithGraphT_spy (asReturns <$> evalStmt Nothing γ sα mempty) (vertices inα)
         when checking.outputsInGraph $ check (vertices outα ⊆ vertices g) "outputs in graph"
         pure (g × inα × outα)
      pure { g, graph_fwd, graph_bwd, inα, outα }
   where
   graph_fwd = curry (fwdSlice # spyFun' tracing.graphFwdSlice "fwdSlice")
   graph_bwd = curry (bwdSlice # spyFun' tracing.graphBwdSlice "bwdSlice")
   spyFun' b msg = spyFunWhen b msg (showVertices *** showGraph) showGraph
