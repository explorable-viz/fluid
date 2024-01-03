module Graph where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (fromFoldable) as A
import Data.Array (uncons)
import Data.Foldable (class Foldable)
import Data.List (List(..), concat, reverse, (:))
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set, singleton, unions)
import Data.Set as Set
import Data.String (joinWith)
import Dict (Dict)
import Dict (apply) as D
import Util (type (×), (\\), (×), (∈), (∪), Endo)

type Edge = Vertex × Vertex
type HyperEdge = Vertex × Set Vertex -- mostly a convenience

-- | Immutable graphs, optimised for lookup and building from (key, value) pairs.
class (Eq g, Vertices g, Semigroup g) <= Graph g where
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
   -- | Construct a graph from initial set of vertices and list of hyperedges (α, βs). Each α is a new
   -- | vertex to be added, and each β in βs already exists in the graph being constructed.
   fromEdgeList :: Set Vertex -> List HyperEdge -> g

newtype Vertex = Vertex String

class Vertices a where
   vertices :: a -> Set Vertex

class Selectαs a b | a -> b where
   selectαs :: a -> b -> Set Vertex
   select𝔹s :: b -> Set Vertex -> a

instance (Functor f, Foldable f) => Vertices (f Vertex) where
   vertices = (singleton <$> _) >>> unions
else instance (Vertices a, Vertices b) => Vertices (a × b) where
   vertices (a × b) = vertices a ∪ vertices b

instance (Functor f, Foldable f) => Vertices (Dict (f Vertex)) where
   vertices = (vertices <$> _) >>> unions

instance (Apply f, Foldable f) => Selectαs (f Boolean) (f Vertex) where
   selectαs v𝔹 vα = unions ((if _ then singleton else const mempty) <$> v𝔹 <*> vα)
   select𝔹s vα αs = (_ ∈ αs) <$> vα
else instance (Selectαs a b, Selectαs a' b') => Selectαs (a × a') (b × b') where
   selectαs (v𝔹 × v𝔹') (vα × vα') = selectαs v𝔹 vα ∪ selectαs v𝔹' vα'
   select𝔹s (vα × vα') αs = select𝔹s vα αs × select𝔹s vα' αs

instance (Functor f, Apply f, Foldable f) => Selectαs (Dict (f Boolean)) (Dict (f Vertex)) where
   selectαs d𝔹 dα = unions ((selectαs <$> d𝔹) `D.apply` dα)
   select𝔹s dα αs = flip select𝔹s αs <$> dα

outEdges' :: forall g. Graph g => g -> Vertex -> List Edge
outEdges' g α = L.fromFoldable $ Set.map (α × _) (outN g α)

outEdges :: forall g. Graph g => g -> Set Vertex -> List Edge
outEdges g αs = concat (outEdges' g <$> L.fromFoldable αs)

inEdges' :: forall g. Graph g => g -> Vertex -> List Edge
inEdges' g α = L.fromFoldable $ Set.map (_ × α) (inN g α)

inEdges :: forall g. Graph g => g -> Set Vertex -> List Edge
inEdges g αs = concat (inEdges' g <$> L.fromFoldable αs)

toEdgeList :: forall g. Graph g => g -> List HyperEdge
toEdgeList g =
   tailRec go (A.fromFoldable (vertices g \\ sinks g) × Nil)
   where
   go :: Array Vertex × List HyperEdge -> Step _ (List HyperEdge)
   go (αs' × acc) = case uncons αs' of
      Nothing -> Done acc
      Just { head: α, tail: αs } -> Loop (αs × (α × outN g α) : acc)

showGraph :: forall g. Graph g => g -> String
showGraph g =
   joinWith "\n" $ [ "digraph G {" ] <> (indent <$> lines) <> [ "}" ]
   where
   lines :: Array String
   lines = [ "rankdir = RL" ] <> edges

   edges :: Array String
   edges = showEdge <$> A.fromFoldable (reverse (toEdgeList g))

   indent :: Endo String
   indent = ("   " <> _)

   showEdge :: HyperEdge -> String
   showEdge (α × αs) =
      unwrap α <> " -> {" <> joinWith ", " (A.fromFoldable $ unwrap `Set.map` αs) <> "}"

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _

instance Show Vertex where
   show = unwrap

showVertices :: Set Vertex -> String
showVertices αs = "{" <> joinWith ", " (A.fromFoldable (unwrap `Set.map` αs)) <> "}"
