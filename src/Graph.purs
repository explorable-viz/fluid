module Graph where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (fromFoldable) as A
import Data.Foldable (class Foldable)
import Data.List (List(..), reverse, uncons, (:))
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set, singleton, unions)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (fst, snd)
import Dict (Dict)
import Foreign.Object (values)
import Lattice (𝔹)
import Util (type (×), Endo, (×))
import Util.Set ((∈))

type Edge = Vertex × Vertex
type HyperEdge = DVertex × Set Vertex -- mostly a convenience

newtype DVertex = DVertex (Vertex × VertexData)

-- | Immutable graphs, optimised for lookup and building from (key, value) pairs. Should think about how this
-- | is different from Data.Graph.
class (Eq g, Vertices g) <= Graph g where
   -- | Whether g contains a given vertex.
   elem :: Vertex -> g -> Boolean

   -- | inN G = outN (op G)
   outN :: g -> Vertex -> Set Vertex
   inN :: g -> Vertex -> Set Vertex

   -- | Number of vertices in g.
   size :: g -> Int

   sources :: g -> Set Vertex
   sinks :: g -> Set Vertex

   -- | op (op g) = g
   op :: Endo g

   empty :: g

   -- | Construct a graph from initial set of sinks and topologically sorted list of hyperedges (α, βs). Read
   -- | right-to-left, each α is a new vertex to be added, and each β ∈ βs already exists in the graph being
   -- | constructed. Upper adjoint to toEdgeList. If "direction" is bwd, hyperedges are assumed to be in
   -- | reverse topological order.
   fromEdgeList :: Set DVertex -> List HyperEdge -> g

   topologicalSort :: g -> List Vertex
   vertexData :: g -> Vertex -> VertexData

newtype Vertex = Vertex String -- so can use directly as dict key

class Selectαs a b | a -> b where
   selectαs :: a -> b -> Set Vertex
   select𝔹s :: b -> Set Vertex -> a

instance (Vertices a) => Vertices (Dict a) where
   vertices d = unions (vertices <$> values (unwrap d))
else instance (Functor f, Foldable f) => Vertices (f DVertex) where
   vertices = Set.fromFoldable

class Vertices a where
   vertices :: a -> Set DVertex

instance (Apply f, Foldable f) => Selectαs (f 𝔹) (f Vertex) where
   selectαs v𝔹 vα = unions ((if _ then singleton else const mempty) <$> v𝔹 <*> vα)
   select𝔹s vα αs = (_ ∈ αs) <$> vα

instance (Functor f, Apply f, Foldable f) => Selectαs (Dict (f 𝔹)) (Dict (f Vertex)) where
   selectαs d𝔹 dα = unions ((selectαs <$> d𝔹) <*> dα)
   select𝔹s dα αs = flip select𝔹s αs <$> dα

outEdges :: forall g. Graph g => g -> Set Vertex -> List Edge
outEdges g = inEdges (op g)

inEdges' :: forall g. Graph g => g -> Vertex -> List Edge
inEdges' g α = L.fromFoldable $ Set.map (_ × α) (inN g α)

inEdges :: forall g. Graph g => g -> Set Vertex -> List Edge
inEdges g αs = tailRec go (L.fromFoldable αs × Nil)
   where
   go :: List Vertex × List Edge -> Step _ (List Edge)
   go (Nil × acc) = Done acc
   go ((α : αs') × acc) = Loop (αs' × (inEdges' g α <> acc))

-- Topologically sorted edge list determining graph.
toEdgeList :: forall g. Graph g => g -> List HyperEdge
toEdgeList g =
   tailRec go (topologicalSort g × Nil)
   where
   go :: List Vertex × List HyperEdge -> Step _ (List HyperEdge)
   go (αs' × acc) = case uncons αs' of
      Nothing -> Done acc
      Just { head: α, tail: αs } -> Loop (αs × ((DVertex (α × vertexData g α)) × outN g α) : acc)

showGraph :: forall g. Graph g => g -> String
showGraph = toEdgeList >>> showEdgeList

showEdgeList :: List HyperEdge -> String
showEdgeList es =
   joinWith "\n" $ [ "digraph G {" ] <> (indent <$> lines) <> [ "}" ]
   where
   lines :: Array String
   lines = [ "rankdir = RL" ] <> edges

   edges :: Array String
   edges = showEdge <$> A.fromFoldable (reverse es)

   indent :: Endo String
   indent = ("   " <> _)

   showEdge :: HyperEdge -> String
   showEdge (DVertex (α × _) × αs) =
      unwrap α <> " -> {" <> joinWith ", " (A.fromFoldable $ unwrap `Set.map` αs) <> "}"

showVertices :: Set Vertex -> String
showVertices αs = "{" <> joinWith ", " (A.fromFoldable (unwrap `Set.map` αs)) <> "}"

showVertices' :: Set DVertex -> String
showVertices' = showVertices <<< addresses

insert' :: Vertex -> Set DVertex -> Set Vertex
insert' α αs = Set.insert α (addresses αs)

dvertex :: forall a. TypeName a => Vertex -> a -> DVertex
dvertex α v = DVertex (α × pack v)

-- ======================
-- Query a graph for a value
-- ======================
runQuery :: forall a g. Ord a => Graph g => (VertexData -> Maybe a) -> g -> Set a
runQuery query g = (query <<< snd <<< unwrap) `Set.mapMaybe` vertices g

-- ======================
-- Packed data associated with Vertex
-- ======================

class TypeName a where
   typeName :: a -> String

newtype VertexData = VertexData (forall r. (forall a. TypeName a => a -> r) -> r)

pack :: forall a. TypeName a => a -> VertexData
pack x = VertexData (\k -> k x)

unpack :: forall r. (forall a. TypeName a => a -> r) -> VertexData -> r
unpack f (VertexData e) = e f

addresses :: Set DVertex -> Set Vertex
addresses = Set.map (fst <<< unwrap)

instance Eq DVertex where
   eq (DVertex (α × _)) (DVertex (α' × _)) = α == α'

instance Ord DVertex where
   compare (DVertex (α × _)) (DVertex (α' × _)) = compare α α'

derive instance Newtype DVertex _

-- ======================
-- boilerplate
-- ======================
derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _
derive newtype instance Show Vertex
