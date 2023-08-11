module Graph.GraphImpl
   ( GraphImpl
   ) where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Data.Set as S
import Data.Foldable (foldl, foldM)
import Data.List (List(..), (:))
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor.Strong (first)
import Dict (Dict)
import Dict as D
import Graph (class Graph, Vertex(..), addOut, op, outN)
import Data.Newtype (unwrap)
import Foreign.Object (runST)
import Foreign.Object.ST as OST
import Foreign.Object.ST (STObject)
import Set (class Set, delete, insert, singleton, union)
import Set as Set
import Util (type (×), (×), definitely)

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
type AdjMap s = Dict (s Vertex)
data GraphImpl s = GraphImpl (AdjMap s) (AdjMap s)

-- Provided for completeness, but for efficiency we avoid them.
instance Set s Vertex => Semigroup (GraphImpl s) where
   append (GraphImpl out1 in1) (GraphImpl out2 in2) =
      GraphImpl (D.unionWith union out1 out2) (D.unionWith union in1 in2)

instance Set s Vertex => Monoid (GraphImpl s) where
   mempty = GraphImpl D.empty D.empty

-- Dict-based implementation with inefficient (linear) add and remove.
instance Set s Vertex => Graph (GraphImpl s) s where
   remove α (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = delete α <$> D.delete (unwrap α) out
      in' = delete α <$> D.delete (unwrap α) in_

   add α αs (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = D.insert (unwrap α) αs out
      in' = foldl (\d α' -> D.insertWith union (unwrap α') (singleton α) d)
         (D.insert (unwrap α) Set.empty in_)
         αs

   addOut α β (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = D.update (insert β >>> Just) (unwrap α)
         (D.insertWith union (unwrap β) Set.empty out)
      in' = D.insertWith union (unwrap β) (singleton α) in_

   addIn α β g = op (addOut β α (op g))

   outN (GraphImpl out _) α = D.lookup (unwrap α) out # definitely "in graph"
   inN g = outN (op g)

   elem (GraphImpl out _) α = isJust (D.lookup (unwrap α) out)
   size (GraphImpl out _) = D.size out

   vertices (GraphImpl out _) = Set.fromFoldable $ S.map Vertex $ D.keys out

   op (GraphImpl out in_) = GraphImpl in_ out

   discreteG αs = GraphImpl discreteM discreteM
      where
      discreteM = D.fromFoldable $ Set.map (\α -> unwrap α × Set.empty) αs

   empty = mempty

   fromFoldable α_αs = GraphImpl out in_
      where
      out = D.fromFoldable (α_αs <#> first unwrap)
      in_ = runST (opMap (L.fromFoldable α_αs)) -- gratuitous to turn one foldable into another

-- In-place update of mutable object to calculate opposite adjacency map.
type MutableAdjMap s r = STObject r (s Vertex)

opMap :: forall s. Set s Vertex => List (Vertex × s Vertex) -> forall r. ST r (MutableAdjMap s r)
opMap α_αs = do
   in_ <- OST.new
   tailRecM addEdges (α_αs × in_)
   where
   addEdges
      :: forall r
       . List (Vertex × s Vertex) × MutableAdjMap s r
      -> ST r (Step (List (Vertex × s Vertex) × MutableAdjMap s r) (MutableAdjMap s r))
   addEdges (Nil × acc) = pure $ Done acc
   addEdges (((α × βs) : rest) × acc) = do
      acc' <- foldM (addEdge α) acc βs
      pure $ Loop (rest × acc')

   addEdge :: forall r. Vertex -> MutableAdjMap s r -> Vertex -> ST r (MutableAdjMap s r)
   addEdge α acc (Vertex β) = do
      αs <- OST.peek β acc <#> case _ of
         Nothing -> singleton α
         Just αs -> insert α αs
      OST.poke β αs acc

instance Show (s Vertex) => Show (GraphImpl s) where
   show (GraphImpl out in_) = "GraphImpl (" <> show out <> " × " <> show in_ <> ")"