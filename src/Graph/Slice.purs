module Graph.Slice where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Map (insert, empty, lookup, delete) as M
import Data.Maybe (maybe)
import Data.Set (Set, empty, insert, singleton, difference)
import Data.Tuple (fst)
import Graph (class Graph, Edge, Vertex, inEdges, inEdges', outN, sinks, op)
import Graph.GraphWriter (WithGraph, extend, runWithGraph)
import Util (type (×), (×))

type PendingVertices = Map Vertex (Set Vertex)

-- | Backward slicing (◁_G)
bwdSlice :: forall g. Graph g => Set Vertex -> g -> g
bwdSlice αs0 g0 = fst $ runWithGraph $ tailRecM go (empty × L.fromFoldable αs0)
   where
   go :: Set Vertex × List Vertex -> WithGraph (Step _ Unit)
   go (_ × Nil) = pure $ Done unit
   go (visited × (α : αs)) = do
      let βs = outN g0 α
      extend α βs
      pure $ Loop ((visited # insert α) × (L.fromFoldable βs <> αs))

-- | De Morgan dual of backward slicing (◁_G)° ≡ Forward slicing on the opposite graph (▷_{G_op})
bwdSliceDualAsFwdOp :: forall g. Graph g => Set Vertex -> g -> g
bwdSliceDualAsFwdOp αs0 g0 = fwdSlice αs0 (op g0)

-- | Forward slicing (▷_G)
fwdSlice :: forall g. Graph g => Set Vertex -> g -> g
fwdSlice αs0 g0 = fst $ runWithGraph $ tailRecM go (M.empty × inEdges g0 αs0)
   where
   go :: PendingVertices × List Edge -> WithGraph (Step _ PendingVertices)
   go (h × Nil) = pure $ Done h
   go (h × ((α × β) : es)) = do
      let βs = maybe (singleton β) (insert β) (M.lookup α h)
      if βs == outN g0 α then do
         extend α βs
         pure $ Loop (M.delete α h × (inEdges' g0 α <> es))
      else
         pure $ Loop (M.insert α βs h × es)

-- | Forward slicing (▷_G) ≡ De Morgan dual of backward slicing on the opposite graph (◁_{G_op})°
-- Also doesn't do the final negation..
fwdSliceAsDeMorgan :: forall g. Graph g => Set Vertex -> g -> g
fwdSliceAsDeMorgan αs0 g0 =
   bwdSlice (sinks g0 `difference` αs0) (op g0)

-- | De Morgan dual of forward slicing (▷_G)°
-- Doesn't do the final negation..
fwdSliceDual :: forall g. Graph g => Set Vertex -> g -> g
fwdSliceDual αs0 g0 = fwdSlice (sinks g0 `difference` αs0) g0

-- | De Morgan dual of Backward slicing ◁_G°
bwdSliceDual :: forall g. Graph g => Set Vertex -> g -> g
bwdSliceDual αs0 g0 = bwdSlice (sources g0 `difference` αs0) g0

-- | De Morgan dual of forward slicing (▷_G)° ≡ Backward slicing on the opposite graph (◁_{G_op})
fwdSliceDualAsBwdOp :: forall g. Graph g => Set Vertex -> g -> g
fwdSliceDualAsBwdOp αs0 g0 = bwdSlice αs0 (op g0)
