module Graph.GraphWriter
   ( class MonadGraphWriter
   , class MonadGraphWriter2
   , AdjMapEntries
   , WithGraph
   , WithGraph2
   , alloc
   , extend
   , fresh
   , new
   , runWithGraph
   , runWithGraph2
   ) where

import Prelude hiding (add)
import Control.Monad.State (State, StateT, runState, modify, modify_)
import Control.Monad.Except (runExceptT)
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first, second)
import Data.Traversable (class Traversable, traverse)
import Graph (Vertex(..), class Graph, fromFoldable)
import Util (MayFailT, MayFail, type (×), (×))

class Monad m <= MonadGraphWriter2 s m | m -> s where
   -- Extend graph with existing vertex pointing to set of existing vertices.
   extend :: Vertex -> s Vertex -> m Unit

class Monad m <= MonadGraphWriter s m | m -> s where
   fresh :: m Vertex
   -- Extend graph with fresh vertex pointing to set of existing vertices; return new vertex.
   new :: s Vertex -> m Vertex

-- Builds list of adjacency map entries (arguments to 'add').
type AdjMapEntries s = List (Vertex × s Vertex)
type WithGraph s a = MayFailT (State (Int × AdjMapEntries s)) a
-- TODO: factor WithGraph through this.
type WithGraph2 s a = State (AdjMapEntries s) a

instance Monad m => MonadGraphWriter s (MayFailT (StateT (Int × AdjMapEntries s) m)) where
   fresh = do
      n × _ <- modify $ first $ (+) 1
      pure (Vertex $ show n)

   new αs = do
      α <- fresh
      modify_ $ second $ (:) (α × αs)
      pure α

instance Monad m => MonadGraphWriter2 s (StateT (AdjMapEntries s) m) where
   extend α αs =
      void $ modify_ $ (:) (α × αs)

alloc :: forall s t a. Traversable t => t a -> WithGraph s (t Vertex)
alloc = traverse (const fresh)

runWithGraph :: forall g s a. Graph g s => WithGraph s a -> MayFail (g × a)
runWithGraph c = ((×) (fromFoldable g_adds)) <$> maybe_r
   where
   maybe_r × _ × g_adds = (flip runState (0 × Nil) <<< runExceptT) c

runWithGraph2 :: forall g s a. Graph g s => WithGraph2 s a -> g × a
runWithGraph2 c = fromFoldable g_adds × a
   where
   a × g_adds = runState c Nil
