module Graph.GraphWriter
   ( AdjMapEntries
   , WithGraph
   , WithGraphT
   , alloc
   , alloc'
   , class MonadGraphWriter
   , fresh
   , new
   , runWithGraph
   , runWithGraphT
   ) where

import Prelude (class Monad, const, pure, bind, show, discard, flip, ($), (+), (<<<), (<>), (<$>))
import Control.Comonad (extract)
import Control.Monad.State.Trans (StateT, runStateT, modify, modify_)
import Control.Monad.Except (runExceptT)
import Data.Identity (Identity)
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
type WithGraphT s m a = MayFailT (StateT (Int × AdjMapEntries s) m) a
type WithGraph s a = WithGraphT s Identity a

instance Monad m => MonadGraphWriter s (MayFailT (StateT (Int × AdjMapEntries s) m)) where
   fresh = do
      n × _ <- modify $ first $ (+) 1
      pure (Vertex $ show n)

   new αs = do
      α <- fresh
      modify_ $ second $ (:) (α × αs)
      pure α

alloc :: forall s m t a. Monad m => Traversable t => t a -> WithGraphT s m (t Vertex)
alloc = traverse (const fresh)

alloc' :: forall s m a. Monad m => a -> WithGraphT s m Vertex
alloc' = const fresh

runWithGraph :: forall g s a. Graph g s => (g × Int) -> WithGraph s a -> MayFail ((g × Int) × a)
runWithGraph (g × n) = extract <<< runWithGraphT (g × n)

runWithGraphT :: forall g s m a. Monad m => Graph g s => (g × Int) -> WithGraphT s m a -> m (MayFail ((g × Int) × a))
runWithGraphT (g × n) e = do
   maybe_r × n' × g_adds <- (flip runStateT (n × Nil) <<< runExceptT) e
   pure $ ((×) ((g <> fromFoldable g_adds) × n')) <$> maybe_r
