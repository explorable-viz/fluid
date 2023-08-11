module Graph.GraphWriter (
   class MonadAlloc,
   class MonadGraphWriter,
   AdjacencyMap,
   WithGraph,
   alloc,
   fresh,
   new,
   runHeap
)
where

import Prelude hiding (add)
import Control.Monad.State (class MonadState, State, StateT, get, modify_, put, runState)
import Control.Monad.Trans.Class (lift)
import Data.List (List, (:))
import Data.Identity (Identity)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst)
import Graph (Vertex(..))
import Util (MayFailT, type (×), (×))

type HeapT m a = StateT Int m a
type Heap a = HeapT Identity a

class MonadState Int m <= MonadAlloc m where
   fresh :: m Vertex

instance Monad m => MonadAlloc (StateT Int m) where
   fresh = do
      s <- get
      put (s + 1)
      pure (Vertex (show s))

{-# Allocating addresses #-}
runHeap :: forall a. Heap a -> a
runHeap = flip runState 0 >>> fst

alloc :: forall t a. Traversable t => t a -> Heap (t Vertex)
alloc = traverse (const fresh)

class Monad m <= MonadGraphWriter s m | m -> s where
   -- Extend graph with fresh vertex pointing to set of existing vertices; return new vertex.
   new :: s Vertex -> m Vertex

-- Builds list of adjacency maplets (arguments to extend).
type AdjacencyMap s = List (Vertex × s Vertex)
type WithGraph s a = MayFailT (StateT (AdjacencyMap s) (State Int)) a

instance MonadAlloc m => MonadGraphWriter s (MayFailT (StateT (AdjacencyMap s) m)) where
   new αs = do
      α <- lift $ lift $ fresh
      modify_ $ (:) (α × αs)
      pure α
