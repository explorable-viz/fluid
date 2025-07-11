module Graph.WithGraph where

import Prelude hiding (map)

import Control.Monad.Except (class MonadError, lift)
import Control.Monad.State (StateT, modify, modify_, runStateT)
import Data.Identity (Identity)
import Data.List (List(..), range, (:))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first, second)
import Data.Set (Set, isEmpty)
import Data.Set as Set
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst, swap)
import Effect.Exception (Error)
import Graph (class Graph, class TypeName, class Vertices, DVertex, DVertex'(..), HyperEdge, Vertex(..), addresses, fromEdgeList, pack, showEdgeList, showGraph, showVertices, toEdgeList, vertices)
import Lattice (Raw)
import Test.Util.Debug (checking, tracing)
import Util (type (×), Endo, assertWhen, check, spy, spyFunWhenM, spyWhen, (×))
import Util.Set ((\\))

data WhichGraph = Deps | Refs

class Monad m <= MonadWithGraphs m where
   addHyperEdge :: DVertex -> Set Vertex -> WhichGraph -> m Unit

class Monad m <= MonadAlloc m where
   fresh :: m Vertex

-- Fix exceptions at Error, the type of JavaScript exceptions, because Aff requires Error, and
-- I can't see a way to convert MonadError Error m (for example) to MonadError Error m.
class (MonadAlloc m, MonadError Error m, MonadWithGraphs m) <= MonadWithGraphsAlloc m where
   new :: forall f g. TypeName (g Vertex) => (Vertex -> f Vertex -> g Vertex) -> Set Vertex -> f Vertex -> m (g Vertex)
   extend :: DVertex -> Set Vertex -> m Unit

type AllocT m = StateT Int m
type Alloc = AllocT Identity
type WithGraphsT = StateT ((List HyperEdge) × (List HyperEdge))
type WithGraphsAllocT m = WithGraphsT (AllocT m)
type WithGraphs = WithGraphsT Identity

instance Monad m => MonadAlloc (AllocT m) where
   fresh = do
      n <- modify $ (+) 1
      pure (Vertex $ show n)

instance Monad m => MonadWithGraphs (WithGraphsT m) where
   addHyperEdge α αs Deps = void $ modify_ $ first ((:) (α × αs))
   addHyperEdge α αs Refs = void $ modify_ $ second ((:) (α × αs))

instance MonadError Error m => MonadWithGraphsAlloc (WithGraphsAllocT m) where
   new constr αs u = do
      α <- fresh
      let v = constr α u
      addHyperEdge (DVertex (α × pack v)) αs Deps
      pure v
   extend α αs = do
      addHyperEdge α αs Refs

alloc :: forall m f. MonadAlloc m => Traversable f => Raw f -> m (f Vertex)
alloc = traverse (const fresh)

runAllocT :: forall m a. Monad m => AllocT m a -> Int -> m (Int × Set Vertex × a)
runAllocT m n = do
   a × n' <- runStateT m n
   let fresh_αs = Set.fromFoldable $ (Vertex <<< show) <$> range' (n + 1) n'
   pure (n' × fresh_αs × a)
   where
   -- built-in range function is singularly useless
   range' :: Int -> Int -> List Int
   range' n1 n2 = if n2 < n1 then Nil else range n1 n2

runWithGraphsT :: forall g m a. Monad m => Graph g => WithGraphsT m a -> Set DVertex -> m (g × g × a)
runWithGraphsT m αs = do
   g × g' × a <- freezeGraphs m αs
   assertWhen checking.edgeListGC "edgeListGC" (\_ -> g == fromEdgeList mempty (toEdgeList g) && g' == fromEdgeList mempty (toEdgeList g')) $
      pure (g × g' × a)

freezeGraphs :: forall g m a. Monad m => Graph g => WithGraphsT m a -> Set DVertex -> m (g × g × a)
freezeGraphs m αs = do
   (es × es') × a <- runStateT m (Nil × Nil) <#> swap
   let g = fromEdgeList αs $ report "edge list" showEdgeList es
   let αs' = vertices g
   let g' = fromEdgeList αs' $ report "edge list'" showEdgeList es'
   pure (g × g' × a)
   where
   report :: forall c b. String -> (c -> b) -> Endo c
   report msg = spyWhen tracing.runWithGraphT ("runWithGraphT " <> msg)

-- ======================
-- Diagnostics
-- ======================

-- Verify round-tripping of x' = alloc x and vertices x'. (Only makes sense if m is ~ alloc x.)
alloc_check :: forall m a. Vertices a => MonadError Error m => String -> AllocT m a -> m Unit
alloc_check msg m = do
   n × αs × x <- runAllocT m 0
   let report = spy (show n <> " allocations, unaccounted for") showVertices
   check (report (αs \\ addresses x) # isEmpty) $ "alloc " <> msg <> " round-trip"

runWithGraphsT_spy :: forall g m a. Monad m => Graph g => WithGraphsT m a -> Set DVertex -> m (g × g × a)
runWithGraphsT_spy = runWithGraphsT
   >>> spyFunWhenM tracing.runWithGraphT "runWithGraphsT" (Set.map (fst <<< unwrap) >>> showVertices) (fst >>> showGraph)

runWithGraphs_spy :: forall g a. Graph g => WithGraphs a -> Set DVertex -> (g × g × a)
runWithGraphs_spy m = runWithGraphsT_spy m >>> unwrap

-- ======================
-- Boilerplate
-- ======================
runAlloc :: forall a. Alloc a -> Int -> Int × Set Vertex × a
runAlloc m = runAllocT m >>> unwrap

instance Monad m => MonadAlloc (WithGraphsAllocT m) where
   fresh = lift fresh
