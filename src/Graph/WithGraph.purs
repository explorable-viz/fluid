module Graph.WithGraph where

import Prelude hiding (map)

import Control.Monad.Except (class MonadError, lift)
import Control.Monad.State (StateT, modify, modify_, runStateT)
import Data.Identity (Identity)
import Data.List (List(..), range, (:))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first)
import Data.Set (Set, isEmpty)
import Data.Set as Set
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst, swap)
import Effect.Exception (Error)
import Graph (class Graph, class Vertices, Direction(..), HyperEdge, Vertex(..), fromEdgeList, showEdgeList, showGraph, showVertices, toEdgeList, vertices)
import Lattice (Raw)
import Test.Util.Debug (checking, tracing)
import Util (type (×), Endo, assertWhen, check, spy, spyFunWhenWithM, spyWhenWith, spyWith, (\\), (×))

class Monad m <= MonadWithGraph m where
   -- Extend graph with existing vertex pointing to set of existing vertices.
   extend :: Vertex -> Set Vertex -> m Unit

class Monad m <= MonadAlloc m where
   fresh :: m Vertex

-- Fix exceptions at Error, the type of JavaScript exceptions, because Aff requires Error, and
-- I can't see a way to convert MonadError Error m (for example) to MonadError Error m.
class (MonadAlloc m, MonadError Error m, MonadWithGraph m) <= MonadWithGraphAlloc m where
   -- Extend with a freshly allocated vertex.
   new :: Set Vertex -> m Vertex

type AllocT m = StateT Int m
type Alloc = AllocT Identity
type WithGraphAllocT m = WithGraphT (AllocT m)
type WithGraphT = StateT (List HyperEdge)
type WithGraph = WithGraphT Identity

instance Monad m => MonadAlloc (AllocT m) where
   fresh = do
      n <- modify $ (+) 1
      pure (Vertex $ show n)

instance MonadError Error m => MonadWithGraphAlloc (WithGraphAllocT m) where
   new αs = do
      α <- fresh
      extend α αs
      pure α

instance Monad m => MonadWithGraph (WithGraphT m) where
   extend α αs = void $ modify_ $ (:) (α × αs)

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

runWithGraphT :: forall g m a. Monad m => Graph g => WithGraphT m a -> Direction -> Set Vertex -> m (g × a)
runWithGraphT m dir αs = do
   g × a <- runStateT m Nil
      <#> swap
      <#> first (fromEdgeList dir αs <<< report "edge list" showEdgeList)
   -- comparing edge lists requires sorting, which causes stack overflow on large graphs
   assertWhen checking.edgeListGC "edgeListGC" (\_ -> g == fromEdgeList Fwd mempty (toEdgeList g)) $
      pure (g × a)
   where
   report :: forall c b. String -> (c -> b) -> Endo c
   report msg = spyWhenWith tracing.runWithGraphT ("runWithGraphT " <> msg)

-- ======================
-- Diagnostics
-- ======================

-- Verify round-tripping of x' = alloc x and vertices x'. (Only makes sense if m is ~ alloc x.)
alloc_check :: forall m a. Vertices a => MonadError Error m => String -> AllocT m a -> m Unit
alloc_check msg m = do
   n × αs × x <- runAllocT m 0
   let report = spyWith (show n <> " allocations, unaccounted for") showVertices
   check (report (αs \\ vertices (spy "Allocated term" x)) # isEmpty) $ "alloc " <> msg <> " round-trip"

runWithGraphT_spy :: forall g m a. Monad m => Graph g => WithGraphT m a -> Direction -> Set Vertex -> m (g × a)
runWithGraphT_spy m = runWithGraphT m
   >>> spyFunWhenWithM tracing.runWithGraphT "runWithGraphT" showVertices (fst >>> showGraph)

-- ======================
-- Boilerplate
-- ======================
runAlloc :: forall a. Alloc a -> Int -> Int × Set Vertex × a
runAlloc m = runAllocT m >>> unwrap

runWithGraph :: forall g a. Graph g => WithGraph a -> Direction -> Set Vertex -> g × a
runWithGraph m dir = runWithGraphT m dir >>> unwrap

instance Monad m => MonadAlloc (WithGraphAllocT m) where
   fresh = lift fresh