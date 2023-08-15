module Graph.GraphWriter
   ( class MonadGraphWriter
   , AdjMapEntries
   , WithGraph
   , alloc
   , fresh
   , new
   , runWithGraph
   ) where

import Prelude hiding (add)
import Control.Monad.State (State, StateT, runState, modify, modify_)
import Control.Monad.Except (runExceptT)
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first, second)
import Data.Traversable (class Traversable, traverse)
import Graph (Vertex(..), class Graph, fromFoldable)
import Util (MayFailT, MayFail, type (×), (×))

class Monad m <= MonadGraphWriter s m | m -> s where
   fresh :: m Vertex
   -- Extend graph with fresh vertex pointing to set of existing vertices; return new vertex.
   new :: s Vertex -> m Vertex

-- Builds list of adjacency map entries (arguments to 'add').
type AdjMapEntries s = List (Vertex × s Vertex)
type WithGraph s a = MayFailT (State (Int × AdjMapEntries s)) a

instance Monad m => MonadGraphWriter s (MayFailT (StateT (Int × AdjMapEntries s) m)) where
   fresh = do
      n × _ <- modify $ first $ (+) 1
      pure (Vertex $ show n)

   new αs = do
      α <- fresh
      modify_ $ second $ (:) (α × αs)
      pure α

alloc :: forall s t a. Traversable t => t a -> WithGraph s (t Vertex)
alloc = traverse (const fresh)

runWithGraph :: forall g s a. Graph g s => WithGraph s a -> MayFail (g × a)
runWithGraph e = ((×) (fromFoldable g_adds)) <$> maybe_r
   where
   maybe_r × _ × g_adds = (flip runState (0 × Nil) <<< runExceptT) e