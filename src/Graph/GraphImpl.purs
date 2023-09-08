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
import Data.Tuple (fst, snd)
import Dict (Dict)
import Dict as D
import Foreign.Object (runST)
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as OST
import Graph (class Graph, Vertex(..), op, outN)
import Set (class Set, insert, singleton)
import Set as Set
import Util (type (×), (×), definitely)

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
type AdjMap s = Dict (s Vertex)
data GraphImpl s = GraphImpl { out :: AdjMap s, in :: AdjMap s }

instance Set s Vertex => Semigroup (GraphImpl s) where
   append (GraphImpl g) (GraphImpl g') =
      GraphImpl { out: D.unionWith Set.union g.out g'.out, in: D.unionWith Set.union g.in g'.in }

-- Dict-based implementation, efficient because Graph doesn't require any update operations.
instance Set s Vertex => Graph (GraphImpl s) s where
   outN (GraphImpl g) α = D.lookup (unwrap α) g.out # definitely "in graph"
   inN g = outN (op g)
   elem α (GraphImpl g) = isJust (D.lookup (unwrap α) g.out)
   size (GraphImpl g) = D.size g.out
   vertices (GraphImpl g) = Set.fromFoldable $ Set.map Vertex $ D.keys g.out
   sinks (GraphImpl g) = sinks' g.out
   sources (GraphImpl g) = sinks' g.in
   op (GraphImpl g) = GraphImpl { out: g.in, in: g.out }
   empty = GraphImpl { out: D.empty, in: D.empty }

   fromFoldable α_αs = GraphImpl { out: runST (outMap α_αs'), in: runST (inMap α_αs') }
      where
      α_αs' = L.fromFoldable α_αs -- doesn't seem to adversely affect performance

-- Naive implementation based on Dict.filter fails with stack overflow on graphs with ~20k vertices.
-- This is better but still slow if there are thousands of sinks.
sinks' :: forall s. Set s Vertex => AdjMap s -> s Vertex
sinks' m = D.toArrayWithKey (×) m
   # A.filter (snd >>> Set.isEmpty)
   <#> (fst >>> Vertex)
   # Set.fromFoldable

-- In-place update of mutable object to calculate opposite adjacency map.
type MutableAdjMap s r = STObject r (s Vertex)

addIfMissing :: forall s r. Set s Vertex => STObject r (s Vertex) -> Vertex -> ST r (MutableAdjMap s r)
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
   show (GraphImpl g) = "GraphImpl (" <> show g.out <> " × " <> show g.in <> ")"
