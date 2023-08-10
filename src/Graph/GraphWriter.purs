module Graph.GraphAccum where

import Prelude hiding (add)
import Control.Monad.State (class MonadState, State, StateT, get, put, runState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT, tell)
import Data.List (List, (:))
import Data.Identity (Identity)
import Data.Profunctor.Strong (first, second)
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

-- Difference graphs
class Monad m <= MonadGraphAccum s m | m -> s where
   -- Extend graph with fresh vertex pointing to set of existing vertices; return new vertex.
   new :: s Vertex -> m Vertex

-- Essentially Writer instantiated to a monoid of endofunctions
data GraphAccumT (g :: Type -> Type) (s :: Type -> Type) m a = GraphAccumT (m (a × Endo (g (s Vertex))))
type WithGraph g s a = MayFailT (GraphAccumT g s (State Int)) a

data GraphAccum2T g m a = GraphAccum2T (g -> m (a × g))
type WithGraph2 g a = MayFailT (GraphAccum2T g (State Int)) a

type GraphExtension s = List (Vertex × s Vertex) -- list of successive arguments to `add`
type WithGraph3 s a = MayFailT (WriterT (Endo (GraphExtension s)) (State Int)) a

runGraphAccumT :: forall g s m a. GraphAccumT g s m a -> m (a × Endo (g (s Vertex)))
runGraphAccumT (GraphAccumT m) = m

runGraphAccum2T :: forall g m a. GraphAccum2T g m a -> g -> m (a × g)
runGraphAccum2T (GraphAccum2T m) = m

instance Functor m => Functor (GraphAccumT g s m) where
   map f (GraphAccumT m) = GraphAccumT $ m <#> first f

instance Functor m => Functor (GraphAccum2T g m) where
   map f (GraphAccum2T m) = GraphAccum2T $ \g -> m g <#> first f

instance Apply m => Apply (GraphAccumT g s m) where
   apply (GraphAccumT m) (GraphAccumT m') = GraphAccumT $ k <$> m <*> m'
      where
      k (f × g) (x × g') = f x × (g >>> g')

instance (Apply m, Monad m) => Apply (GraphAccum2T g m) where
   apply = ap

instance Bind m => Bind (GraphAccumT g s m) where
   bind (GraphAccumT m) f = GraphAccumT $ do
      x × g <- m
      let GraphAccumT m' = f x
      m' <#> second ((>>>) g)

instance Monad m => Bind (GraphAccum2T g m) where
   bind (GraphAccum2T x) f = GraphAccum2T \g ->
      x g >>= \(y × g') -> case f y of GraphAccum2T x' -> x' g'

instance (Monoid (g (s Vertex)), Applicative m) => Applicative (GraphAccumT g s m) where
   pure a = GraphAccumT $ pure $ a × identity

instance Monad m => Applicative (GraphAccum2T g m) where
   pure x = GraphAccum2T \g -> pure $ x × g

instance (Monoid (g (s Vertex)), Monad m) => Monad (GraphAccumT g s m)

instance Monad m => Monad (GraphAccum2T g m)

instance Monoid (g (s Vertex)) => MonadTrans (GraphAccumT g s) where
   lift m = GraphAccumT $ (×) <$> m <@> identity

instance Monoid g => MonadTrans (GraphAccum2T g) where
   lift m = GraphAccum2T \g -> (×) <$> m <@> g

instance (Graph (g (s Vertex)) s, MonadAlloc m) => MonadGraphAccum s (GraphAccumT g s m) where
   new αs = do
      α <- lift $ fresh
      GraphAccumT $ pure $ α × add α αs

instance (Graph g s, MonadAlloc m) => MonadGraphAccum s (GraphAccum2T g m) where
   new αs = do
      α <- lift $ fresh
      GraphAccum2T $ \g -> pure $ α × add α αs g

instance MonadAlloc m => MonadGraphAccum s (MayFailT (WriterT (Endo (GraphExtension s)) m)) where
   new αs = do
      α <- lift $ lift $ fresh
      tell $ (:) (α × αs)
      pure α
