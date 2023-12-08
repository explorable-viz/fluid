module Graph where

import Prelude hiding (add)

import Data.Foldable (class Foldable)
import Data.List (List, concat)
import Data.List (fromFoldable) as L
import Data.Newtype (class Newtype)
import Data.Set (Set, singleton, unions)
import Data.Set (empty, map) as S
import Data.Set.NonEmpty (NonEmptySet)
import Dict (Dict)
import Util (Endo, (×), type (×), (∈))

type Edge = Vertex × Vertex

-- | Immutable graphs, optimised for lookup and building from (key, value) pairs.
class (Vertices g, Semigroup g) <= Graph g where
   -- | Whether g contains a given vertex.
   elem :: Vertex -> g -> Boolean
   -- | outN and iN satisfy
   -- |   inN G = outN (op G)
   outN :: g -> Vertex -> Set Vertex
   inN :: g -> Vertex -> Set Vertex

   -- | Number of vertices in g.
   size :: g -> Int

   sources :: g -> Set Vertex
   sinks :: g -> Set Vertex

   -- | op (op g) = g
   op :: Endo g

   empty :: g
   fromFoldable :: forall f. Functor f => Foldable f => f (Vertex × NonEmptySet Vertex) -> g

newtype Vertex = Vertex String

class Vertices a where
   vertices :: a -> Set Vertex

instance (Functor f, Foldable f) => Vertices (f Vertex) where
   vertices = (singleton <$> _) >>> unions

instance (Functor f, Foldable f) => Vertices (Dict (f Vertex)) where
   vertices = (vertices <$> _) >>> unions

selectαs :: forall f. Apply f => Foldable f => f Boolean -> f Vertex -> Set Vertex
selectαs v𝔹 vα = unions ((if _ then singleton else const S.empty) <$> v𝔹 <*> vα)

select𝔹s :: forall f. Functor f => f Vertex -> Set Vertex -> f Boolean
select𝔹s vα αs = (_ ∈ αs) <$> vα

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
