module Graph.Slice where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Map (insert, empty, lookup, delete) as M
import Data.Maybe (maybe)
import Data.Set (Set, empty, insert, member, singleton, unions, difference)
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
bwdSliceDual :: forall g. Graph g => Set Vertex -> g -> g
bwdSliceDual αs0 g0 = fwdSlice αs0 (op g0)

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

-- | De Morgan dual of forward slicing (▷_G)° ≡ Backward slicing on the opposite graph (◁_{G_op})
fwdSliceDual :: forall g. Graph g => Set Vertex -> g -> g
fwdSliceDual αs0 g0 = bwdSlice αs0 (op g0)

-- | Forward slicing (▷_G) ≡ De Morgan dual of backward slicing on the opposite graph (◁_{G_op})°
fwdSliceAsDeMorgan :: forall g. Graph g => Set Vertex -> g -> g
fwdSliceAsDeMorgan αs0 g0 =
   bwdSlice (sinks g0 `difference` αs0) (op g0)

vertices :: forall f. Apply f => Foldable f => f Vertex -> Set Vertex
vertices vα = selectαs (const true <$> vα) vα

selectαs :: forall f. Apply f => Foldable f => f Boolean -> f Vertex -> Set Vertex
selectαs v𝔹 vα = unions ((if _ then singleton else const empty) <$> v𝔹 <*> vα)

select𝔹s :: forall f. Functor f => f Vertex -> Set Vertex -> f Boolean
select𝔹s vα αs = (_ `member` αs) <$> vα
