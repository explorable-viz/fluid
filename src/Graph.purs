module Graph where

import Prelude hiding (add)

import Data.Foldable (class Foldable)
import Data.List (List, concat)
import Data.List (fromFoldable) as L
import Data.Newtype (class Newtype)
import Set (class Set)
import Set (map) as Set
import Util (Endo, (×), type (×))

type Edge = Vertex × Vertex

-- | "Static" graphs, optimised for lookup and building from (key, value) pairs.
class (Monoid g, Set s Vertex) <= Graph g s | g -> s where
   -- | Whether g contains a given vertex.
   elem :: Vertex -> g -> Boolean
   -- | outN and iN satisfy
   -- |   inN G = outN (op G)
   outN :: g -> Vertex -> s Vertex
   inN :: g -> Vertex -> s Vertex

   -- | Number of vertices in g.
   size :: g -> Int

   -- | Set of all vertices in g
   vertices :: g -> s Vertex
   sources :: g -> s Vertex
   sinks :: g -> s Vertex
   -- |   op (op g) = g
   op :: Endo g

   empty :: g

   fromFoldable :: forall f. Functor f => Foldable f => f (Vertex × s Vertex) -> g

newtype Vertex = Vertex String

outEdges' :: forall g s. Graph g s => g -> Vertex -> List Edge
outEdges' g α = L.fromFoldable $ Set.map (α × _) (outN g α)

outEdges :: forall g s. Graph g s => g -> s Vertex -> List Edge
outEdges g αs = concat (outEdges' g <$> L.fromFoldable αs)

inEdges' :: forall g s. Graph g s => g -> Vertex -> List Edge
inEdges' g α = L.fromFoldable $ Set.map (_ × α) (inN g α)

inEdges :: forall g s. Graph g s => g -> s Vertex -> List Edge
inEdges g αs = concat (inEdges' g <$> L.fromFoldable αs)

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _

instance Show Vertex where
   show (Vertex α) = "Vertex " <> α
