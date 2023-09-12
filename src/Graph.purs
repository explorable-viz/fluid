module Graph where

import Prelude hiding (add)

import Data.Foldable (class Foldable)
import Data.List (List, concat)
import Data.List (fromFoldable) as L
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Util (Endo, (×), type (×))

type Edge = Vertex × Vertex

-- | "Static" graphs, optimised for lookup and building from (key, value) pairs.
class Semigroup g <= Graph g where
   -- | Whether g contains a given vertex.
   elem :: Vertex -> g -> Boolean
   -- | outN and inN satisfy
   -- |   inN G = outN (op G)
   outN :: g -> Vertex -> Set Vertex
   inN :: g -> Vertex -> Set Vertex

   -- | Number of vertices in g.
   size :: g -> Int
   vertices :: g -> Set Vertex

   sources :: g -> Set Vertex
   sinks :: g -> Set Vertex

   -- |   op (op g) = g
   op :: Endo g

   empty :: g
   fromFoldable :: forall f. Functor f => Foldable f => f (Vertex × Set Vertex) -> g

newtype Vertex = Vertex String

outEdges' :: forall g. Graph g => g -> Vertex -> List Edge
outEdges' g α = L.fromFoldable $ S.map (α × _) (outN g α)

outEdges :: forall g. Graph g => g -> Set Vertex -> List Edge
outEdges g αs = concat (outEdges' g <$> L.fromFoldable αs)

inEdges' :: forall g. Graph g => g -> Vertex -> List Edge
inEdges' g α = L.fromFoldable $ S.map (_ × α) (inN g α)

inEdges :: forall g. Graph g => g -> Set Vertex -> List Edge
inEdges g αs = concat (inEdges' g <$> L.fromFoldable αs)

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _

instance Show Vertex where
   show (Vertex α) = "Vertex " <> α
