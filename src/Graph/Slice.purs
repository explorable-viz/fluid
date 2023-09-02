module Graph.Slice where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Map (insert, empty, lookup, delete) as M
import Data.Maybe (maybe)
import Data.Set (Set)
import Data.Tuple (fst)
import Graph (class Graph, Edge, Vertex, inEdges, inEdges', outN, sinks, op)
import Graph.GraphWriter (WithGraph, extend, runWithGraph)
import Set (empty, insert, member, singleton, unions, difference)
import Util (type (×), (×))

type PendingSlice s = Map Vertex (s Vertex)

bwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
bwdSlice αs0 g0 = fst $ runWithGraph $ tailRecM go (empty × L.fromFoldable αs0)
   where
   go :: (s Vertex × List Vertex) -> WithGraph s (Step _ Unit)
   go (_ × Nil) = pure $ Done unit
   go (visited × (α : αs)) = do
      let βs = outN g0 α
      extend α βs
      pure $ Loop ((visited # insert α) × (L.fromFoldable βs <> αs))

fwdSliceDeMorgan :: forall g s. Graph g s => s Vertex -> g -> g
fwdSliceDeMorgan αs_0 g_0 =
   bwdSlice (sinks g_0 `difference` αs_0) (op g_0)

fwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
fwdSlice αs0 g0 = fst $ runWithGraph $ tailRecM go (M.empty × inEdges g0 αs0)
   where
   go :: (PendingSlice s × List Edge) -> WithGraph s (Step _ (PendingSlice s))
   go (h × Nil) = pure $ Done h
   go (h × ((α × β) : es)) = do
      let βs = maybe (singleton β) (insert β) (M.lookup α h)
      if βs == outN g0 α then do
         extend α βs
         pure $ Loop ((M.delete α h) × (inEdges' g0 α <> es))
      else
         pure $ Loop ((M.insert α βs h) × es)

selectαs :: forall f. Apply f => Foldable f => f Boolean -> f Vertex -> Set Vertex
selectαs v𝔹 vα = unions ((if _ then singleton else const empty) <$> v𝔹 <*> vα)

select𝔹s :: forall f. Functor f => f Vertex -> Set Vertex -> f Boolean
select𝔹s vα αs = flip member αs <$> vα
