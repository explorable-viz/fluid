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
import Data.Tuple (swap)
import Debug (trace)
import Effect.Exception (Error)
import Graph (class Graph, class Vertices, HyperEdge, Vertex(..), fromEdgeList, showEdgeList, showGraph, showVertices, toEdgeList, vertices)
import Lattice (Raw)
import Test.Util.Debug (checking, tracing)
import Util (type (×), assertWhen, check, spyWhenWith, spyWith, (\\), (×))

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

runAllocT :: forall m a. Monad m => Int -> AllocT m a -> m (Int × Set Vertex × a)
runAllocT n m = do
   a × n' <- runStateT m n
   let fresh_αs = Set.fromFoldable $ (Vertex <<< show) <$> range (n + 1) n'
   pure (n' × fresh_αs × a)

-- Verify round-tripping of x' = alloc x and vertices x'. (Only makes sense if m is of the form alloc x.)
alloc_check :: forall m a. Vertices a => MonadError Error m => String -> AllocT m a -> m Unit
alloc_check msg m = do
   n × αs × x <- runAllocT 0 m
   trace x \_ ->
      check ((spyWith ("Of " <> show n <> " allocations, unaccounted for") showVertices (αs \\ vertices x)) # isEmpty) $
         "alloc " <> msg <> " round-trip"

runWithGraphT :: forall g m a. Monad m => Graph g => Set Vertex -> WithGraphT m a -> m (g × a)
runWithGraphT αs m = do
   g × a <- runStateT m Nil <#> swap <#> first (\es -> fromEdgeList αs (spyWith "edgeList" showEdgeList es))
   -- comparing edge lists requires sorting, which causes stack overflow on large graphs
   assertWhen checking.edgeListIso "edgeListIso" (\_ -> g == fromEdgeList αs (toEdgeList g)) $
      pure ((spyWhenWith tracing.graphCreation "runWithGraphT" showGraph g) × a)

-- ======================
-- Boilerplate
-- ======================
runAlloc :: forall a. Int -> Alloc a -> Int × Set Vertex × a
runAlloc n = runAllocT n >>> unwrap

runWithGraph :: forall g a. Graph g => Set Vertex -> WithGraph a -> g × a
runWithGraph αs = runWithGraphT αs >>> unwrap

instance Monad m => MonadAlloc (WithGraphAllocT m) where
   fresh = lift fresh
