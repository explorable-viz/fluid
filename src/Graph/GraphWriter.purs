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
import Data.Profunctor.Strong (first)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (swap)
import Data.Set (Set)
import Graph (Vertex(..), class Graph, fromFoldable)
import Util (MayFailT, type (×), type (+), (×))

class Monad m <= MonadGraph m where
   -- Extend graph with existing vertex pointing to set of existing vertices.
   extend :: Vertex -> Set Vertex -> m Unit

class Monad m <= MonadAlloc m where
   fresh :: m Vertex

class (MonadAlloc m, MonadGraph m) <= MonadGraphAlloc m where
   -- Extend with a freshly allocated vertex.
   new :: Set Vertex -> m Vertex

-- List of adjacency map entries to serve as a fromFoldable input.
type AdjMapEntries = List (Vertex × Set Vertex)
type WithAllocT m = StateT Int m
type WithAlloc = WithAllocT Identity
type WithGraphAllocT m = MayFailT (WithAllocT (WithGraphT m))
type WithGraphT = StateT AdjMapEntries
type WithGraph = WithGraphT Identity

instance Monad m => MonadAlloc (WithAllocT m) where
   fresh = do
      n <- modify $ (+) 1
      pure (Vertex $ show n)

instance Monad m => MonadAlloc (WithGraphAllocT m) where
   fresh = lift fresh

instance (Monad m, MonadAlloc (WithGraphAllocT m), MonadGraph (WithGraphAllocT m)) => MonadGraphAlloc  (WithGraphAllocT m) where
   new αs = do
      α <- fresh
      extend α αs
      pure α

instance Monad m => MonadGraph (WithGraphT m) where
   extend α αs =
      void $ modify_ $ (:) (α × αs)

instance Monad m => MonadGraph (WithGraphAllocT m) where
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

runWithGraphT :: forall g m a. Monad m => Graph g => WithGraphT m a -> m (g × a)
runWithGraphT c = runStateT c Nil <#> swap <#> first fromFoldable

runWithGraph :: forall g a. Graph g => WithGraph a -> g × a
runWithGraph = runWithGraphT >>> unwrap

runWithGraphAllocT :: forall g m a. Monad m => Graph g => g × Int -> WithGraphAllocT m a -> m (String + ((g × Int) × a))
runWithGraphAllocT (g × n) c = do
   (n' × maybe_a) × g_adds <- runStateT (runWithAllocT n (runExceptT c)) Nil
   pure $ maybe_a <#> (((g <> fromFoldable g_adds) × n') × _)
