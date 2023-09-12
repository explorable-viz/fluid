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
import Data.Set (Set, union)
import Data.Set (empty, fromFoldable, insert, isEmpty, map, singleton) as S
import Data.Tuple (fst, snd)
import Dict (Dict)
import Dict as D
import Foreign.Object (runST)
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as OST
import Graph (class Graph, Vertex(..), op, outN)
import Util (type (×), (×), definitely)

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
type AdjMap = Dict (Set Vertex)
data GraphImpl = GraphImpl { out :: AdjMap, in :: AdjMap }

instance Semigroup GraphImpl where
   append (GraphImpl g) (GraphImpl g') =
      GraphImpl { out: D.unionWith union g.out g'.out, in: D.unionWith union g.in g'.in }

-- Dict-based implementation, efficient because Graph doesn't require any update operations.
instance Graph GraphImpl where
   outN (GraphImpl g) α = D.lookup (unwrap α) g.out # definitely "in graph"
   inN g = outN (op g)
   elem α (GraphImpl g) = isJust (D.lookup (unwrap α) g.out)
   size (GraphImpl g) = D.size g.out
   vertices (GraphImpl g) = S.fromFoldable $ S.map Vertex $ D.keys g.out
   sinks (GraphImpl g) = sinks' g.out
   sources (GraphImpl g) = sinks' g.in
   op (GraphImpl g) = GraphImpl { out: g.in, in: g.out }
   empty = GraphImpl { out: D.empty, in: D.empty }

   fromFoldable α_αs = GraphImpl { out: runST (outMap α_αs'), in: runST (inMap α_αs') }
      where
      α_αs' = L.fromFoldable α_αs -- doesn't seem to adversely affect performance

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

outMap :: List (Vertex × Set Vertex) -> forall r. ST r (MutableAdjMap r)
outMap α_αs = do
   out <- OST.new
   tailRecM addEdges (α_αs × out)
   where
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((α × βs) : rest) × acc) = do
      acc' <- OST.poke (unwrap α) βs acc >>= flip (foldM addIfMissing) βs
      pure $ Loop (rest × acc')

inMap :: List (Vertex × Set Vertex) -> forall r. ST r (MutableAdjMap r)
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
         Nothing -> OST.poke β (S.singleton α) acc
         Just αs -> OST.poke β (S.insert α αs) acc

instance Show (s Vertex) => Show GraphImpl where
   show (GraphImpl g) = "GraphImpl (" <> show g.out <> " × " <> show g.in <> ")"
