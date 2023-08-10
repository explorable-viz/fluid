module SliceGraph where

import Prelude
import Data.List (List(..), (:))
import Data.List as L
import Data.Set (Set)
import Data.Tuple (fst)
import Graph (class Graph, Edge, Vertex, addIn, addOut, discreteG, elem, extend, inE, inE', outE, outE', outN, remove)
import Util (type (×), (×))

bwdSlice :: forall g. Graph g => Set Vertex -> g -> g
bwdSlice αs g' = bwdEdges g' (discreteG αs) (outE αs g')

bwdEdges :: forall g. Graph g => g -> g -> List Edge -> g
bwdEdges g' g ((α × β) : es) =
   bwdEdges g' (addOut α β g) $
      es <> if elem g β then Nil else L.fromFoldable (outE' g' β)
bwdEdges _ g Nil = g

fwdSlice :: forall g. Graph g => Set Vertex -> g -> g
fwdSlice αs g' = fst $ fwdEdges g' (discreteG αs) mempty (inE αs g')

fwdEdges :: forall g. Graph g => g -> g -> g -> List Edge -> g × g
fwdEdges g' g h ((α × β) : es) = fwdEdges g' g'' h' es
   where
   g'' × h' = fwdVertex g' g (addIn α β h) α
fwdEdges _ currSlice pending Nil = currSlice × pending

fwdVertex :: forall g. Graph g => g -> g -> g -> Vertex -> g × g
fwdVertex g' g h α =
   if αs == outN g' α then
      fwdEdges g' (extend α αs g) (remove α h) (inE' g' α)
   else g × h
   where
   αs = outN h α
