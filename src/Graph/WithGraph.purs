module Graph.WithGraph where

import Prelude hiding (map)

import Control.Monad.Except (class MonadError, lift)
import Control.Monad.State (StateT, modify, modify_, runStateT)
import Data.Identity (Identity)
import Data.List (List(..), range, (:))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (swap)
import Effect.Exception (Error)
import Graph (class Graph, Vertex(..), HyperEdge, fromEdgeList, showGraph, toEdgeList)
import Lattice (Raw)
import Test.Util.Debug (checking, tracing)
import Util (type (×), assertWhen, spyWhen, (×))

class Monad m <= MonadWithGraph m where
   -- Extend graph with existing vertex pointing to set of existing vertices.
   extend :: Vertex -> NonEmptySet Vertex -> m Unit

class Monad m <= MonadAlloc m where
   fresh :: m Vertex

-- Fix exceptions at Error, the type of JavaScript exceptions, because Aff requires Error, and
-- I can't see a way to convert MonadError Error m (for example) to MonadError Error m.
class (MonadAlloc m, MonadError Error m, MonadWithGraph m) <= MonadWithGraphAlloc m where
   -- Extend with a freshly allocated vertex.
   new :: NonEmptySet Vertex -> m Vertex

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
   extend α αs =
      void $ modify_ $ (:) (α × αs)

alloc :: forall m t. MonadAlloc m => Traversable t => Raw t -> m (t Vertex)
alloc = traverse (const fresh)

runAllocT :: forall m a. Monad m => Int -> AllocT m a -> m (Int × Set Vertex × a)
runAllocT n m = do
   a × n' <- runStateT m n
   let fresh_αs = Set.fromFoldable $ (Vertex <<< show) <$> range (n + 1) n'
   pure (n' × fresh_αs × a)

runWithGraphT :: forall g m a. Monad m => Graph g => WithGraphT m a -> m (g × a)
runWithGraphT m = do
   g × a <- runStateT m Nil <#> swap <#> first fromEdgeList
   -- comparing edge lists requires sorting, which causes stack overflow on large graphs
   assertWhen checking.edgeListIso (\_ -> g == fromEdgeList (toEdgeList g)) $
      pure ((spyWhen tracing.graphCreation "runWithGraphT" showGraph g) × a)

-- ======================
-- Boilerplate
-- ======================
runAlloc :: forall a. Int -> Alloc a -> Int × Set Vertex × a
runAlloc n = runAllocT n >>> unwrap

runWithGraph :: forall g a. Graph g => WithGraph a -> g × a
runWithGraph = runWithGraphT >>> unwrap

instance Monad m => MonadAlloc (WithGraphAllocT m) where
   fresh = lift fresh
