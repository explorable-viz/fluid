module Graph.GraphWriter
   ( class MonadGraphWriter
   , AdjMapEntries
   , WithGraph
   , alloc
   , fresh
   , new
   ) where

import Prelude hiding (add)
import Control.Monad.State (State, StateT, modify, modify_)
import Data.List (List, (:))
import Data.Profunctor.Strong (first, second)
import Data.Traversable (class Traversable, traverse)
import Graph (Vertex(..))
import Util (MayFailT, type (×), (×))

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

alloc :: forall s t a. Traversable t => t a -> MayFailT (State (Int × AdjMapEntries s)) (t Vertex)
alloc = traverse (const fresh)
