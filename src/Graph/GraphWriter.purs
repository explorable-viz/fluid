module Graph.GraphWriter
   ( AdjMapEntries
   , WithGraphT
   , WithGraph2
   , WithGraph2T
   , class MonadGraphAlloc
   , class MonadGraphWriter
   , alloc
   , extend
   , fresh
   , new
   , runWithGraph2
   , runWithGraph2T
   , runWithGraphT
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

class Monad m <= MonadGraphAlloc s m | m -> s where
   fresh :: m Vertex
   -- Extend with a freshly allocated vertex.
   new :: s Vertex -> m Vertex

-- List of adjacency map entries to serve as a fromFoldable input.
type AdjMapEntries s = List (Vertex × s Vertex)
type WithGraphT s m = MayFailT (StateT Int (WithGraph2T s m))
type WithGraph2T s = StateT (AdjMapEntries s)
type WithGraph2 s = WithGraph2T s Identity

instance Monad m => MonadGraphAlloc s (WithGraphT s m) where
   fresh = do
      n <- modify $ (+) 1
      pure (Vertex $ show n)

   new αs = do
      α <- fresh
      lift $ lift $ modify_ $ (:) (α × αs)
      pure α

instance Monad m => MonadGraphWriter s (WithGraph2T s m) where
   extend α αs =
      void $ modify_ $ (:) (α × αs)

alloc :: forall s m t a. Monad m => Traversable t => t a -> WithGraphT s m (t Vertex)
alloc = traverse (const fresh)

runWithGraph2T :: forall g s m a. Monad m => Graph g s => WithGraph2T s m a -> m (g × a)
runWithGraph2T c = runStateT c Nil <#> swap <#> first fromFoldable

runWithGraph2 :: forall g s a. Graph g s => WithGraph2 s a -> g × a
runWithGraph2 c = unwrap $ runWithGraph2T c

runWithGraphT :: forall g s m a. Monad m => Graph g s => (g × Int) -> WithGraphT s m a -> m (MayFail ((g × Int) × a))
runWithGraphT (g × n) c = do
   (maybe_a × n') × g_adds <- runStateT (runStateT (runExceptT c) n) Nil
   pure $ maybe_a <#> ((×) ((g <> fromFoldable g_adds) × n'))
