module DependenceGraph where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (empty, map, singleton, union, unions) as S
import Data.Tuple (Tuple(..), fst, snd, swap)
import Foreign.Object (Object, delete, fromFoldableWith, insert, lookup, singleton, size, toUnfoldable) as O
import Util (Endo)

class Graph g where
   union :: Vertex -> Set Vertex -> Endo g
   outN :: g -> Vertex -> Set Vertex
   singleton :: Vertex -> Set Vertex -> g
   remove :: Vertex -> Endo g
   opp :: Endo g
   allocate :: g -> Vertex

newtype Vertex = Vertex String

unwrap :: Vertex -> String
unwrap (Vertex string) = string

newtype AnnGraph = AnnGraph (O.Object (Tuple Vertex (Set Vertex)))

newtype SimpleGraph = SimpleGraph (O.Object (Set Vertex))

instance Graph SimpleGraph where
   allocate (SimpleGraph obj) = Vertex id
      where
      id = show $ 1 + (O.size obj)
   remove (Vertex key) (SimpleGraph obj) = SimpleGraph (O.delete key obj)
   union (Vertex key) neighbs (SimpleGraph obj) = (SimpleGraph newObj)
      where
      newObj = O.insert key neighbs obj
   outN (SimpleGraph obj) (Vertex key) = case O.lookup key obj of
      Just verts -> verts
      Nothing -> S.empty
   singleton (Vertex key) neighbs = SimpleGraph (O.singleton key neighbs)
   opp s = fromEdges $ reverseEdges $ allEdges s

-- SimpleGraph $ foldrWithIndex combine obj verts
--   where
--   combine v edges = O.unionWith S.union (O.fromFoldable edges)
--   verts = O.keys obj
allEdges :: SimpleGraph -> Set (Tuple Vertex Vertex)
allEdges (SimpleGraph obj) = let out = (map adjEdges (O.toUnfoldable obj :: Array (Tuple String (Set Vertex)))) in S.unions out

adjEdges :: Tuple String (Set Vertex) -> Set (Tuple Vertex Vertex)
adjEdges (Tuple id neighbs) = adjEdges' (Vertex id) neighbs

adjEdges' :: Vertex -> Set Vertex -> Set (Tuple Vertex Vertex)
adjEdges' v neighbs = S.map (\node -> (Tuple v node)) neighbs

reverseEdges :: Endo (Set (Tuple Vertex Vertex))
reverseEdges edges = S.map swap edges

fromEdges :: Set (Tuple Vertex Vertex) -> SimpleGraph
fromEdges edges = SimpleGraph $
   O.fromFoldableWith S.union (S.map (\pair -> Tuple (unwrap (fst pair)) (S.singleton $ snd pair)) edges)

-- instance Foldable Graph where
--     foldl f z (Graph o) = Graph (foldl f z (map fst (O.values o) :: ?_))
--     foldr f z (Graph o) = Graph (foldr f z o)
--     foldMap f (Graph o) = Graph (foldMap f o)

derive instance Eq Vertex
derive instance Ord Vertex