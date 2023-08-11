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
inEdges' g α = L.fromFoldable $ Set.map (_ × α) (inN g α)

inEdges :: forall g s. Graph g s => g -> s Vertex -> List Edge
inEdges g αs = concat (inEdges' g <$> L.fromFoldable αs)

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _

instance Show Vertex where
   show (Vertex α) = "Vertex " <> α
