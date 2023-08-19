module Graph.GraphWriter
   ( AdjMapEntries
   , WithGraphAllocT
   , WithGraph
   , WithGraphT
   , class MonadGraphAlloc
   , class MonadGraphWriter
   , alloc
   , extend
   , fresh
   , new
   , runWithGraph
   , runWithGraphT
   , runWithGraphAllocT
   ) where

import Prelude
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Trans (StateT, lift, modify, modify_, runStateT)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Newtype (unwrap)
import Data.Tuple (swap)
import Data.Profunctor.Strong (first)
import Data.Traversable (class Traversable, traverse)
import Graph (Vertex(..), class Graph, fromFoldable)
import Util (MayFailT, MayFail, type (×), (×))

class Monad m <= MonadGraphWriter s m | m -> s where
   -- Extend graph with existing vertex pointing to set of existing vertices.
   extend :: Vertex -> s Vertex -> m Unit

class MonadGraphWriter s m <= MonadGraphAlloc s m | m -> s where
   fresh :: m Vertex
   -- Extend with a freshly allocated vertex.
   new :: s Vertex -> m Vertex

-- List of adjacency map entries to serve as a fromFoldable input.
type AdjMapEntries s = List (Vertex × s Vertex)
type WithGraphAllocT s m = MayFailT (StateT Int (WithGraphT s m))
type WithGraphT s = StateT (AdjMapEntries s)
type WithGraph s = WithGraphT s Identity

instance (Monad m, MonadGraphWriter s (WithGraphAllocT s m)) => MonadGraphAlloc s (WithGraphAllocT s m) where
   fresh = do
      n <- modify $ (+) 1
      pure (Vertex $ show n)

   new αs = do
      α <- fresh
      extend α αs
      pure α

instance Monad m => MonadGraphWriter s (WithGraphT s m) where
   extend α αs =
      void $ modify_ $ (:) (α × αs)

instance Monad m => MonadGraphWriter s (WithGraphAllocT s m) where
   extend α αs = lift $ lift $ extend α αs

alloc :: forall s m t a. Monad m => Traversable t => t a -> WithGraphAllocT s m (t Vertex)
alloc = traverse (const fresh)

runWithGraphT :: forall g s m a. Monad m => Graph g s => WithGraphT s m a -> m (g × a)
runWithGraphT c = runStateT c Nil <#> swap <#> first fromFoldable

runWithGraph :: forall g s a. Graph g s => WithGraph s a -> g × a
runWithGraph = runWithGraphT >>> unwrap

runWithGraphAllocT :: forall g s m a. Monad m => Graph g s => g × Int -> WithGraphAllocT s m a -> m (MayFail ((g × Int) × a))
runWithGraphAllocT (g × n) c = do
   (maybe_a × n') × g_adds <- runStateT (runStateT (runExceptT c) n) Nil
   pure $ maybe_a <#> (((g <> fromFoldable g_adds) × n') × _)
