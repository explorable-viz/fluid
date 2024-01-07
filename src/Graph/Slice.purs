module Graph.Slice where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (maybe)
import Data.Set (Set, empty, insert)
import Data.Tuple (fst)
import Graph (class Graph, Edge, Vertex, inEdges, inEdges', outN)
import Graph.WithGraph (WithGraph, extend, runWithGraph)
import Util (type (×), singleton, (×), (∈))

type PendingVertices = Map Vertex (Set Vertex)

bwdSlice :: forall g. Graph g => Set Vertex × g -> g
bwdSlice (αs × g) =
   fst (runWithGraph αs $ tailRecM go (empty × L.fromFoldable αs))
   where
   go :: Set Vertex × List Vertex -> WithGraph (Step _ Unit)
   go (_ × Nil) = Done <$> pure unit
   go (visited × (α : αs')) = Loop <$>
      if α ∈ visited then
         pure (visited × αs')
      else do
         let βs = outN g α
         extend α βs
         pure (insert α visited × (L.fromFoldable βs <> αs'))

fwdSlice :: forall g. Graph g => (Set Vertex × g) -> g
fwdSlice (αs × g) =
   fst (runWithGraph αs $ tailRecM go (M.empty × inEdges g αs))
   where
   go :: PendingVertices × List Edge -> WithGraph (Step _ PendingVertices)
   go (h × Nil) = Done <$> pure h
   go (h × ((α × β) : es)) = Loop <$>
      if βs == outN g α then do
         extend α βs
         pure (M.delete α h × (inEdges' g α <> es))
      else
         pure (M.insert α βs h × es)
      where
      βs = maybe (singleton β) (insert β) (lookup α h)
