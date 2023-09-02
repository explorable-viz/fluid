module Graph.GraphWriter
   ( AdjMapEntries
   , WithAllocT
   , WithGraphAllocT
   , WithGraph
   , WithGraphT
   , class MonadAlloc
   , class MonadGraphAlloc
   , class MonadGraph
   , alloc
   , extend
   , fresh
   , new
   , runWithAlloc
   , runWithAllocT
   , runWithGraph
   , runWithGraphT
   , runWithGraphAllocT
   ) where

import Prelude
import Control.Monad.Except (runExceptT)
import Control.Monad.State (StateT, runState, runStateT, modify, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Newtype (unwrap)
import Data.Tuple (swap)
import Data.Profunctor.Strong (first)
import Data.Traversable (class Traversable, traverse)
import Graph (Vertex(..), class Graph, fromFoldable)
import Util (MayFailT, type (×), type (+), (×))

class Monad m <= MonadGraph s m | m -> s where
   -- Extend graph with existing vertex pointing to set of existing vertices.
   extend :: Vertex -> s Vertex -> m Unit

class Monad m <= MonadAlloc m where
   fresh :: m Vertex

class (MonadAlloc m, MonadGraph s m) <= MonadGraphAlloc s m | m -> s where
   -- Extend with a freshly allocated vertex.
   new :: s Vertex -> m Vertex

-- List of adjacency map entries to serve as a fromFoldable input.
type AdjMapEntries s = List (Vertex × s Vertex)
type WithAllocT m = StateT Int m
type WithAlloc = WithAllocT Identity
type WithGraphAllocT s m = MayFailT (WithAllocT (WithGraphT s m))
type WithGraphT s = StateT (AdjMapEntries s)
type WithGraph s = WithGraphT s Identity

instance Monad m => MonadAlloc (WithAllocT m) where
   fresh = do
      n <- modify $ (+) 1
      pure (Vertex $ show n)

instance Monad m => MonadAlloc (WithGraphAllocT s m) where
   fresh = lift fresh

instance (Monad m, MonadAlloc (WithGraphAllocT s m), MonadGraph s (WithGraphAllocT s m)) => MonadGraphAlloc s (WithGraphAllocT s m) where
   new αs = do
      α <- fresh
      extend α αs
      pure α

instance Monad m => MonadGraph s (WithGraphT s m) where
   extend α αs =
      void $ modify_ $ (:) (α × αs)

instance Monad m => MonadGraph s (WithGraphAllocT s m) where
   extend α = lift <<< lift <<< extend α

alloc :: forall m t a. MonadAlloc m => Traversable t => t a -> m (t Vertex)
alloc = traverse (const fresh)

-- TODO: make synonymous with runStateT/runState?
runWithAllocT :: forall m a. Monad m => Int -> WithAllocT m a -> m (Int × a)
runWithAllocT n c = do
   a × n' <- runStateT c n
   pure $ n' × a

runWithAlloc :: forall a. Int -> WithAlloc a -> Int × a
runWithAlloc n c = runState c n # swap

runWithGraphT :: forall g s m a. Monad m => Graph g s => WithGraphT s m a -> m (g × a)
runWithGraphT c = runStateT c Nil <#> swap <#> first fromFoldable

runWithGraph :: forall g s a. Graph g s => WithGraph s a -> g × a
runWithGraph = runWithGraphT >>> unwrap

runWithGraphAllocT :: forall g s m a. Monad m => Graph g s => g × Int -> WithGraphAllocT s m a -> m (String + ((g × Int) × a))
runWithGraphAllocT (g × n) c = do
   (n' × maybe_a) × g_adds <- runStateT (runWithAllocT n (runExceptT c)) Nil
   pure $ maybe_a <#> (((g <> fromFoldable g_adds) × n') × _)
