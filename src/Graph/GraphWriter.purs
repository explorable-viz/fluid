module Graph.GraphWriter where

import Prelude hiding (add)
import Control.Monad.State (class MonadState, State, StateT, get, put, runState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT, tell)
import Data.List (List, (:))
import Data.Identity (Identity)
import Data.Profunctor.Strong (first, second)
import Data.Set (Set)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst)
import Graph (class Graph, Vertex(..), add)
import Util (Endo, MayFailT, type (×), (×))

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

class Monad m <= MonadGraphWriter m where
   -- Extend graph with fresh vertex pointing to set of existing vertices; return new vertex.
   new :: Set Vertex -> m Vertex

-- Ιmplementation #1
-- Essentially Writer instantiated to a monoid of endofunctions
data GraphWriterT g m a = GraphWriterT (m (a × Endo g))
type WithGraph g a = MayFailT (GraphWriterT g (State Int)) a

runGraphWriterT :: forall g m a. GraphWriterT g m a -> m (a × Endo g)
runGraphWriterT (GraphWriterT m) = m

instance Functor m => Functor (GraphWriterT g m) where
   map f (GraphWriterT m) = GraphWriterT $ m <#> first f

instance Apply m => Apply (GraphWriterT g m) where
   apply (GraphWriterT m) (GraphWriterT m') = GraphWriterT $ k <$> m <*> m'
      where
      k (f × g) (x × g') = f x × (g >>> g')

instance Bind m => Bind (GraphWriterT g m) where
   bind (GraphWriterT m) f = GraphWriterT $ do
      x × g <- m
      let GraphWriterT m' = f x
      m' <#> second ((>>>) g)

instance (Monoid g, Applicative m) => Applicative (GraphWriterT g m) where
   pure a = GraphWriterT $ pure $ a × identity

instance (Monoid g, Monad m) => Monad (GraphWriterT g m)

instance Monoid g => MonadTrans (GraphWriterT g) where
   lift m = GraphWriterT $ (×) <$> m <@> identity

instance (Graph g, MonadAlloc m) => MonadGraphWriter (GraphWriterT g m) where
   new αs = do
      α <- lift $ fresh
      GraphWriterT $ pure $ α × add α αs

instance (Graph g, MonadAlloc m) => MonadGraphWriter (GraphWriter2T g m) where
   new αs = do
      α <- lift $ fresh
      GraphWriter2T $ \g -> pure $ α × add α αs g

-- Ιmplementation #2
-- State-based rather than Writer-based.
data GraphWriter2T g m a = GraphWriter2T (g -> m (a × g))
type WithGraph2 g a = MayFailT (GraphWriter2T g (State Int)) a

runGraphWriter2T :: forall g m a. GraphWriter2T g m a -> g -> m (a × g)
runGraphWriter2T (GraphWriter2T m) = m

instance Functor m => Functor (GraphWriter2T g m) where
   map f (GraphWriter2T m) = GraphWriter2T $ \g -> m g <#> first f

instance (Apply m, Monad m) => Apply (GraphWriter2T g m) where
   apply = ap

instance Monad m => Bind (GraphWriter2T g m) where
   bind (GraphWriter2T x) f = GraphWriter2T \g ->
      x g >>= \(y × g') -> case f y of GraphWriter2T x' -> x' g'

instance Monad m => Applicative (GraphWriter2T g m) where
   pure x = GraphWriter2T \g -> pure $ x × g

instance Monad m => Monad (GraphWriter2T g m)

instance Monoid g => MonadTrans (GraphWriter2T g) where
   lift m = GraphWriter2T \g -> (×) <$> m <@> g

-- Ιmplementation #3
-- Also Writer-based; builds list of adjacency maplets (arguments to extend).
type AdjacencyMap = List (Vertex × Set Vertex)
type WithGraph3 a = MayFailT (WriterT (Endo AdjacencyMap) (State Int)) a

instance MonadAlloc m => MonadGraphWriter (MayFailT (WriterT (Endo AdjacencyMap) m)) where
   new αs = do
      α <- lift $ lift $ fresh
      tell $ (:) (α × αs)
      pure α
