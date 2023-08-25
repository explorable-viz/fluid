module Graph.Slice where

import Prelude hiding (add)

import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, lookup, delete, insertWith)
import Data.Map (empty) as M
import Data.Set (Set)
import Data.Tuple (fst)
import Graph (class Graph, Edge, Vertex, inEdges, inEdges', outN)
import Graph.GraphWriter (WithGraph, extend, runWithGraph)
import Set (class Set, empty, insert, member, singleton, union, unions)
import Util ((×), definitely)

type PendingSlice s = Map Vertex (s Vertex)

bwdSlice :: forall g s. Set s Vertex => Graph g s => s Vertex -> g -> g
bwdSlice αs g' =
   fst $ runWithGraph $ bwdVertices g' empty (L.fromFoldable αs)

bwdVertices :: forall g s. Graph g s => g -> s Vertex -> List Vertex -> WithGraph s Unit
bwdVertices _ _ Nil = pure unit
bwdVertices g' visited (α : αs) =
   if α `member` visited then bwdVertices g' visited αs
   else do
      let βs = outN g' α
      extend α βs
      bwdVertices g' (visited # insert α) (L.fromFoldable βs <> αs)

fwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
fwdSlice αs g' =
   fst $ runWithGraph $ fwdEdges g' M.empty (inEdges g' αs)

fwdEdges :: forall g s. Graph g s => g -> PendingSlice s -> List Edge -> WithGraph s (PendingSlice s)
fwdEdges _ pending Nil = pure pending
fwdEdges g' h ((α × β) : es) = do
   h' <- fwdVertex g' (insertWith union α (singleton β) h) α
   fwdEdges g' h' es

fwdVertex :: forall g s. Set s Vertex => Graph g s => g -> PendingSlice s -> Vertex -> WithGraph s (PendingSlice s)
fwdVertex g' h α =
   if αs == outN g' α then do
      extend α αs
      fwdEdges g' (delete α h) (inEdges' g' α)
   else pure h
   where
   αs = lookup α h # definitely "in pending map"

selectVertices :: forall f. Apply f => Foldable f => f Boolean -> f Vertex -> Set Vertex
selectVertices v𝔹 vα = unions (asSet <$> v𝔹 <*> vα)

select𝔹s :: forall f. Functor f => f Vertex -> Set Vertex -> f Boolean
select𝔹s vα αs = flip member αs <$> vα

asSet :: forall s. Set s Vertex => Boolean -> Vertex -> s Vertex
asSet true = singleton
asSet false = const empty
