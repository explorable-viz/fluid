module Graph.GraphImpl where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Data.Filterable (filter)
import Data.Graph as G
import Data.List (List(..), reverse, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Set (Set, insert)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Dict (Dict)
import Dict as D
import Foreign.Object (runST)
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as OST
import Graph (class Graph, class Vertices, HyperEdge, Vertex(..), op, outN)
import Test.Util.Debug (checking)
import Util (type (×), assertWhen, definitely, error, isEmpty, singleton, (×))
import Util.Map (keys, lookup, toUnfoldable)
import Util.Set (empty, size)

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
type AdjMap = Dict (Set Vertex)

data GraphImpl = GraphImpl
   { out :: AdjMap
   , in_ :: AdjMap
   , sinks :: Set Vertex
   , sources :: Set Vertex
   , vertices :: Set Vertex
   }

instance Eq GraphImpl where
   eq (GraphImpl g) (GraphImpl g') = g.out == g'.out

-- Dict-based implementation, efficient because Graph doesn't require any update operations.
instance Graph GraphImpl where
   outN (GraphImpl g) α = lookup (unwrap α) g.out # definitely "in graph"
   inN g = outN (op g)
   elem α (GraphImpl g) = isJust (lookup (unwrap α) g.out)
   size (GraphImpl g) = size g.out
   sinks (GraphImpl g) = g.sinks
   sources (GraphImpl g) = g.sources
   op (GraphImpl g) = GraphImpl { out: g.in_, in_: g.out, sinks: g.sources, sources: g.sinks, vertices: g.vertices }
   empty = GraphImpl { out: empty, in_: empty, sinks: mempty, sources: mempty, vertices: mempty }

   fromEdgeList αs es =
      GraphImpl { out, in_, sinks: sinks' out, sources: sinks' in_, vertices }
      where
      es' = reverse es
      αs' = L.fromFoldable αs
      out = wrap (runST (outMap αs' es'))
      in_ = wrap (runST (inMap αs' es'))
      vertices = Set.fromFoldable $ Set.map Vertex $ keys out

   -- PureScript also provides a graph implementation. Delegate to that for now.
   topologicalSort (GraphImpl g) =
      reverse (G.topologicalSort (G.fromMap (M.fromFoldable (kvs <#> (Vertex *** (unit × _))))))
      where
      kvs :: Array (String × List Vertex)
      kvs = toUnfoldable (g.out <#> Set.toUnfoldable)

instance Vertices GraphImpl where
   vertices (GraphImpl g) = g.vertices

-- Naive implementation based on Dict.filter fails with stack overflow on graphs with ~20k vertices.
-- This is better but still slow if there are thousands of sinks.
sinks' :: AdjMap -> Set Vertex
sinks' m = D.toArrayWithKey (×) (unwrap m)
   # filter (snd >>> isEmpty)
   <#> (fst >>> Vertex)
   # Set.fromFoldable

-- In-place update of mutable object to calculate opposite adjacency map.
type MutableAdjMap r = STObject r (Set Vertex)

assertPresent :: forall r. MutableAdjMap r -> List Vertex -> ST r (Step (List Vertex) Unit)
assertPresent _ Nil = pure $ Done unit
assertPresent obj (Vertex α : αs) = do
   present <- OST.peek α obj <#> isJust
   assertWhen checking.edgeListSorted (α <> " is an existing vertex") (\_ -> present)
      $ pure
      $ Loop αs

addIfMissing :: forall r. MutableAdjMap r -> Vertex -> ST r (MutableAdjMap r)
addIfMissing acc (Vertex α) =
   OST.peek α acc >>= case _ of
      Nothing -> OST.poke α mempty acc
      Just _ -> pure acc

addIfMissing' :: forall r. List Vertex -> MutableAdjMap r -> ST r (MutableAdjMap r)
addIfMissing' αs acc = flip tailRecM (αs × acc) case _ of
   (Nil × acc') -> pure $ Done acc'
   ((α : βs) × acc') -> do
      acc'' <- addIfMissing acc' α
      pure $ Loop (βs × acc'')

init :: forall r. List Vertex -> ST r (MutableAdjMap r)
init αs = do
   obj <- OST.new
   tailRecM go (αs × obj)
   where
   go :: List _ × MutableAdjMap r -> ST r (Step _ _)
   go (Nil × acc) = pure $ Done acc
   go ((Vertex α : αs') × acc) = do
      acc' <- OST.poke α mempty acc
      pure $ Loop (αs' × acc')

outMap :: forall r. List Vertex -> List HyperEdge -> ST r (MutableAdjMap r)
outMap αs es = do
   out <- init αs
   tailRecM addEdges (es × out)
   where
   addEdges :: List HyperEdge × MutableAdjMap r -> ST r (Step _ (MutableAdjMap r))
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((Vertex α × βs) : es') × acc) = do
      ok <- OST.peek α acc <#> maybe true (_ == mempty)
      if ok then do
         let βs' = Set.toUnfoldable βs
         tailRecM (assertPresent acc) βs'
         acc' <- OST.poke α βs acc >>= addIfMissing' βs'
         pure $ Loop (es' × acc')
      else
         error $ "Duplicate edge list entry for " <> show α

inMap :: forall r. List Vertex -> List HyperEdge -> ST r (MutableAdjMap r)
inMap αs es = do
   in_ <- init αs
   tailRecM addEdges (es × in_)
   where
   addEdges :: List HyperEdge × MutableAdjMap r -> ST r (Step _ (MutableAdjMap r))
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((α × βs) : es') × acc) = do
      acc' <- tailRecM (addEdge' α) (Set.toUnfoldable βs × acc) >>= flip addIfMissing α
      pure $ Loop (es' × acc')

   addEdge :: Vertex -> MutableAdjMap r -> Vertex -> ST r (MutableAdjMap r)
   addEdge α acc (Vertex β) = do
      OST.peek β acc >>= case _ of
         Nothing -> OST.poke β (singleton α) acc
         Just αs' -> OST.poke β (insert α αs') acc

   addEdge' :: Vertex -> List Vertex × MutableAdjMap r -> ST r (Step _ (MutableAdjMap r))
   addEdge' _ (Nil × acc) = pure $ Done acc
   addEdge' α ((β : βs) × acc) = do
      acc' <- addEdge α acc β
      pure $ Loop (βs × acc')
