module Graph where

import Prelude hiding (add)

import Data.Foldable (foldl, class Foldable)
import Data.List (List, concat)
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as S
import Data.Unfoldable (class Unfoldable)
import Dict (Dict)
import Dict as D
import Util (Endo, (×), type (×), definitely)

type Edge = Vertex × Vertex

-- | Graphs form a semigroup but we don't actually rely on that (for efficiency).
class (Monoid g, Set s Vertex) <= Graph g s | g -> s where
   -- add vertex α to g with αs as out neighbours, where each neighbour is already in g.
   -- | add and remove satisfy:
   -- |    remove α (add α αs g) = g
   -- |    add α (outN α g) (remove α g) = g
   add :: Vertex -> s Vertex -> Endo g

   -- remove a vertex from g.
   remove :: Vertex -> Endo g

   -- addOut α β adds β as new out-neighbour of existing vertex α, adding into g if necessary
   -- | addIn and addOut satisfy
   -- |   addIn α β G = op (addOut β α (op G)
   addOut :: Vertex -> Vertex -> Endo g
   -- | addIn α β adds α as new in-neighbour of existing vertex β, adding into g if necessary
   addIn :: Vertex -> Vertex -> Endo g

   -- | Whether g contains a given vertex.
   elem :: g -> Vertex -> Boolean

   -- | outN and iN satisfy
   -- |   inN G = outN (op G)
   outN :: g -> Vertex -> s Vertex
   inN :: g -> Vertex -> s Vertex

   -- | Number of vertices in g.
   size :: g -> Int

   -- | s of all vertices in g
   vertices :: g -> s Vertex
   -- |   op (op g) = g
   op :: Endo g

   -- |   Discrete graph consisting only of a set of vertices.
   discreteG :: s Vertex -> g

   empty :: g

newtype Vertex = Vertex String

outEdges' :: forall g s. Graph g s => g -> Vertex -> List Edge
outEdges' g = inEdges' (op g)

outEdges :: forall g s. Graph g s => g -> s Vertex -> List Edge
outEdges g = inEdges (op g)

inEdges' :: forall g s. Graph g s => g -> Vertex -> List Edge
inEdges' g α = L.fromFoldable $ smap (_ × α) (inN g α)

inEdges :: forall g s. Graph g s => g -> s Vertex -> List Edge
inEdges g αs = concat (inEdges' g <$> L.fromFoldable αs)

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _

instance Show Vertex where
   show (Vertex α) = "Vertex " <> α

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
data GraphImpl s = GraphImpl (Dict (s Vertex)) (Dict (s Vertex))

type GraphSet = GraphImpl S.Set

-- Provided for completeness, but for efficiency we avoid them.
instance Set s Vertex => Semigroup (GraphImpl s) where
   append (GraphImpl out1 in1) (GraphImpl out2 in2) =
      GraphImpl (D.unionWith union out1 out2) (D.unionWith union in1 in2)

instance Set s Vertex => Monoid (GraphImpl s) where
   mempty = GraphImpl D.empty D.empty

instance Set s Vertex => Graph (GraphImpl s) s where
   remove α (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = delete α <$> D.delete (unwrap α) out
      in' = delete α <$> D.delete (unwrap α) in_

   add α αs (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = D.insert (unwrap α) αs out
      in' = foldl (\d α' -> D.insertWith union (unwrap α') (singleton α) d)
         (D.insert (unwrap α) sempty in_)
         αs

   addOut α β (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = D.update (insert β >>> Just) (unwrap α)
         (D.insertWith union (unwrap β) sempty out)
      in' = D.insertWith union (unwrap β) (singleton α) in_

   addIn α β g = op (addOut β α (op g))

   outN (GraphImpl out _) α = D.lookup (unwrap α) out # definitely "in graph"
   inN g = outN (op g)

   elem (GraphImpl out _) α = isJust (D.lookup (unwrap α) out)
   size (GraphImpl out _) = D.size out

   vertices (GraphImpl out _) = fromFoldable $ S.map Vertex $ D.keys out

   op (GraphImpl out in_) = GraphImpl in_ out

   discreteG αs = GraphImpl discreteM discreteM
      where
      discreteM = D.fromFoldable $ smap (\α -> unwrap α × sempty) αs

   empty = mempty

instance Show (s Vertex) => Show (GraphImpl s) where
   show (GraphImpl out in_) = "GraphImpl (" <> show out <> " × " <> show in_ <> ")"

class (Ord a, Ord (s a), Foldable s) <= Set s a where
   delete :: a -> s a -> s a
   union :: s a -> s a -> s a
   insert :: a -> s a -> s a
   member :: a -> s a -> Boolean
   subset :: s a -> s a -> Boolean
   singleton :: a -> s a
   sempty :: s a
   smap :: forall b. Ord b => (a -> b) -> s a -> s b
   fromFoldable :: forall f. Foldable f => f a -> s a
   toUnfoldable :: forall f. Unfoldable f => s a -> f a

instance Ord a => Set S.Set a where
   delete = S.delete
   union = S.union
   insert = S.insert
   member = S.member
   singleton = S.singleton
   subset = S.subset
   sempty = S.empty
   smap = S.map
   fromFoldable = S.fromFoldable
   toUnfoldable = S.toUnfoldable
