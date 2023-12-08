module Graph.GraphImpl
   ( GraphImpl(..)
   , AdjMap
   ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Data.Array as A
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Set (Set, insert, singleton)
import Data.Set as S
import Data.Set.NonEmpty (NonEmptySet, toSet)
import Data.Tuple (fst, snd)
import Dict (Dict)
import Dict as D
import Foreign.Object (runST)
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as OST
import Graph (class Graph, class Vertices, Vertex(..), op, outN)
import Util (type (×), definitely, error, (\\), (×), (∩), (∪))

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
type AdjMap = Dict (Set Vertex)
data GraphImpl = GraphImpl
   { out :: AdjMap
   , in :: AdjMap
   , sinks :: Set Vertex
   , sources :: Set Vertex
   , vertices :: Set Vertex
   }

instance Semigroup GraphImpl where
   append (GraphImpl g) (GraphImpl g') = GraphImpl
      { out: D.unionWith (∪) g.out g'.out
      , in: D.unionWith (∪) g.in g'.in
      , sinks: (g.sinks ∩ g'.sinks) ∪ (g.sinks \\ g'.vertices) ∪ (g'.sinks \\ g.vertices)
      , sources: (g.sources ∩ g'.sources) ∪ (g.sources \\ g'.vertices) ∪ (g'.sources \\ g.vertices)
      , vertices: g.vertices ∪ g'.vertices
      }

-- Dict-based implementation, efficient because Graph doesn't require any update operations.
instance Graph GraphImpl where
   outN (GraphImpl g) α = D.lookup (unwrap α) g.out # definitely "in graph"
   inN g = outN (op g)
   elem α (GraphImpl g) = isJust (D.lookup (unwrap α) g.out)
   size (GraphImpl g) = D.size g.out
   sinks (GraphImpl g) = g.sinks
   sources (GraphImpl g) = g.sources
   op (GraphImpl g) = GraphImpl { out: g.in, in: g.out, sinks: g.sources, sources: g.sinks, vertices: g.vertices }
   empty = GraphImpl { out: D.empty, in: D.empty, sinks: S.empty, sources: S.empty, vertices: S.empty }

   -- Last entry will take priority if keys are duplicated in α_αs.
   fromEdgeList _ α_αs = GraphImpl { out, in: in_, sinks: sinks' out, sources: sinks' in_, vertices }
      where
      out = runST (outMap α_αs')
      in_ = runST (inMap α_αs')
      vertices = S.fromFoldable $ S.map Vertex $ D.keys out
      α_αs' = L.fromFoldable α_αs -- doesn't seem to adversely affect performance

instance Vertices GraphImpl where
   vertices (GraphImpl g) = g.vertices

-- Naive implementation based on Dict.filter fails with stack overflow on graphs with ~20k vertices.
-- This is better but still slow if there are thousands of sinks.
sinks' :: AdjMap -> Set Vertex
sinks' m = D.toArrayWithKey (×) m
   # A.filter (snd >>> S.isEmpty)
   <#> (fst >>> Vertex)
   # S.fromFoldable

-- In-place update of mutable object to calculate opposite adjacency map.
type MutableAdjMap r = STObject r (Set Vertex)

addIfMissing :: forall r. STObject r (Set Vertex) -> Vertex -> ST r (MutableAdjMap r)
addIfMissing acc (Vertex β) = do
   OST.peek β acc >>= case _ of
      Nothing -> OST.poke β S.empty acc
      Just _ -> pure acc

outMap :: List (Vertex × NonEmptySet Vertex) -> forall r. ST r (MutableAdjMap r)
outMap α_αs = do
   out <- OST.new
   tailRecM addEdges (α_αs × out)
   where
   addEdges :: List (Vertex × NonEmptySet Vertex) × MutableAdjMap _ -> ST _ _
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((Vertex α × βs) : rest) × acc) = do
      ok <- OST.peek α acc <#> case _ of
         Nothing -> true
         Just βs' -> S.isEmpty βs'
      if ok then do
         acc' <- OST.poke α (toSet βs) acc >>= flip (foldM addIfMissing) βs
         pure $ Loop (rest × acc')
      else error $ "Duplicate key " <> α

inMap :: List (Vertex × NonEmptySet Vertex) -> forall r. ST r (MutableAdjMap r)
inMap α_αs = do
   in_ <- OST.new
   tailRecM addEdges (α_αs × in_)
   where
   addEdges :: List (Vertex × NonEmptySet Vertex) × MutableAdjMap _ -> ST _ _
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((α × βs) : rest) × acc) = do
      acc' <- foldM (addEdge α) acc βs >>= flip addIfMissing α
      pure $ Loop (rest × acc')

   addEdge α acc (Vertex β) = do
      OST.peek β acc >>= case _ of
         Nothing -> OST.poke β (singleton α) acc
         Just αs -> OST.poke β (insert α αs) acc

instance Show GraphImpl where
   show (GraphImpl g) = "GraphImpl (" <> show g.out <> " × " <> show g.in <> ")"
