module Graph.GraphImpl
   ( GraphImpl(..)
   , AdjMap
   ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Set as S
import Dict (Dict)
import Dict as D
import Foreign.Object (runST, filter)
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as OST
import Graph (class Graph, Vertex(..), op, outN)
import Set (class Set, insert, singleton, union)
import Set as Set
import Util (type (×), (×), definitely, error, unimplemented)

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
type AdjMap s = Dict (s Vertex)
data GraphImpl s = GraphImpl (AdjMap s) (AdjMap s)

data GraphImpl2 s = GraphImpl2 {
   out :: AdjMap s,
   in :: AdjMap s,
   sources :: s Vertex,
   sinks :: s Vertex
}

instance Set s Vertex => Semigroup (GraphImpl2 s) where
   append (GraphImpl2 g) (GraphImpl2 g') =
      GraphImpl2 {
         out: D.unionWith union g.out g'.out,
         in: D.unionWith union g.in g'.in,
         sources: error unimplemented,
         sinks: error unimplemented
      }

instance Set s Vertex => Graph (GraphImpl2 s) s where
   outN (GraphImpl2 g) α = D.lookup (unwrap α) g.out # definitely "in graph"
   inN g = outN (op g)
   elem α (GraphImpl2 g) = isJust (D.lookup (unwrap α) g.out)
   size (GraphImpl2 g) = D.size g.out
   vertices (GraphImpl2 g) = Set.fromFoldable $ S.map Vertex $ D.keys g.out
   sinks (GraphImpl2 g) = g.sinks
   sources (GraphImpl2 g) = g.sources
   op (GraphImpl2 g) = GraphImpl2 { out: g.in, in: g.out, sources: g.sinks, sinks: g.sources }
   empty = GraphImpl2 { out: D.empty, in: D.empty, sources: Set.empty, sinks: Set.empty }

   fromFoldable _ = error unimplemented

instance Set s Vertex => Semigroup (GraphImpl s) where
   append (GraphImpl out1 in1) (GraphImpl out2 in2) =
      GraphImpl (D.unionWith Set.union out1 out2) (D.unionWith Set.union in1 in2)

-- Dict-based implementation, efficient because Graph doesn't require any update operations.
instance Set s Vertex => Graph (GraphImpl s) s where
   outN (GraphImpl out _) α = D.lookup (unwrap α) out # definitely "in graph"
   inN g = outN (op g)

   elem α (GraphImpl out _) = isJust (D.lookup (unwrap α) out)
   size (GraphImpl out _) = D.size out

   vertices (GraphImpl out _) = Set.fromFoldable $ S.map Vertex $ D.keys out
   sinks (GraphImpl out _) = Set.fromFoldable $ S.map Vertex $ D.keys (filter Set.isEmpty out)
   sources (GraphImpl _ in_) = Set.fromFoldable $ S.map Vertex $ D.keys (filter Set.isEmpty in_)

   op (GraphImpl out in_) = GraphImpl in_ out

   empty = GraphImpl D.empty D.empty

   fromFoldable α_αs = GraphImpl out in_
      where
      α_αs' = L.fromFoldable α_αs
      out × in_ = runST (outMap α_αs') × runST (inMap α_αs')

-- In-place update of mutable object to calculate opposite adjacency map.
type MutableAdjMap s r = STObject r (s Vertex)

addIfMissing :: forall s r. Set s Vertex => STObject r (s Vertex) -> Vertex -> ST r (STObject r (s Vertex))
addIfMissing acc (Vertex β) = do
   OST.peek β acc >>= case _ of
      Nothing -> OST.poke β Set.empty acc
      Just _ -> pure acc

outMap :: forall s. Set s Vertex => List (Vertex × s Vertex) -> forall r. ST r (MutableAdjMap s r)
outMap α_αs = do
   out <- OST.new
   tailRecM addEdges (α_αs × out)
   where
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((α × βs) : rest) × acc) = do
      acc' <- OST.poke (unwrap α) βs acc >>= flip (foldM addIfMissing) βs
      pure $ Loop (rest × acc')

inMap :: forall s. Set s Vertex => List (Vertex × s Vertex) -> forall r. ST r (MutableAdjMap s r)
inMap α_αs = do
   in_ <- OST.new
   tailRecM addEdges (α_αs × in_)
   where
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((α × βs) : rest) × acc) = do
      acc' <- foldM (addEdge α) acc βs >>= flip addIfMissing α
      pure $ Loop (rest × acc')

   addEdge α acc (Vertex β) = do
      OST.peek β acc >>= case _ of
         Nothing -> OST.poke β (singleton α) acc
         Just αs -> OST.poke β (insert α αs) acc

instance Show (s Vertex) => Show (GraphImpl s) where
   show (GraphImpl out in_) = "GraphImpl (" <> show out <> " × " <> show in_ <> ")"
