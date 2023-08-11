module Graph where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Data.Foldable (class Foldable, foldl, foldM)
import Data.List (List(..), (:), concat)
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (first)
import Data.Set (Set, map) as S
import Dict (Dict)
import Dict as D
import Foreign.Object (runST)
import Foreign.Object.ST as OST
import Foreign.Object.ST (STObject)
import Set (class Set, delete, insert, sempty, singleton, smap, union)
import Set (fromFoldable) as S
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

   fromFoldable :: forall f. Functor f => Foldable f => f (Vertex × s Vertex) -> g

newtype Vertex = Vertex String

outEdges' :: forall g s. Graph g s => g -> Vertex -> List Edge
outEdges' g α = L.fromFoldable $ smap (α × _) (outN g α)

outEdges :: forall g s. Graph g s => g -> s Vertex -> List Edge
outEdges g αs = concat (outEdges' g <$> L.fromFoldable αs)

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
type AdjMap s = Dict (s Vertex)
data GraphImpl s = GraphImpl (AdjMap s) (AdjMap s)
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

   vertices (GraphImpl out _) = S.fromFoldable $ S.map Vertex $ D.keys out

   op (GraphImpl out in_) = GraphImpl in_ out

   discreteG αs = GraphImpl discreteM discreteM
      where
      discreteM = D.fromFoldable $ smap (\α -> unwrap α × sempty) αs

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
