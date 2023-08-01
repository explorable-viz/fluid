module DependenceGraph where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (empty, map, singleton, union, unions) as S
import Data.Tuple (Tuple(..), fst, snd, swap)
import Foreign.Object (Object, delete, fromFoldableWith, insert, lookup, singleton, size, toUnfoldable) as O
import Util (Endo, (×), type (×))

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

newtype AnnGraph = AnnGraph (O.Object (Vertex × (Set Vertex)))

newtype GraphImpl = GraphImpl (O.Object (Set Vertex))

instance Graph GraphImpl where
   allocate (GraphImpl obj) = Vertex α 
      where
      α = show $ 1 + (O.size obj)
   remove (Vertex α) (GraphImpl obj) = GraphImpl (O.delete α obj)
   union (Vertex α) αs (GraphImpl obj) = (GraphImpl newObj)
      where
      newObj = O.insert α αs obj
   outN (GraphImpl obj) (Vertex α) = case O.lookup α obj of
      Just αs -> αs
      Nothing -> S.empty
   singleton (Vertex α) αs = GraphImpl (O.singleton α αs)
   opp s = fromEdges $ reverseEdges $ allEdges s

-- GraphImpl $ foldrWithIndex combine obj αs
--   where
--   combine v edges = O.unionWith S.union (O.fromFoldable edges)
--   αs = O.αs obj
allEdges :: GraphImpl -> Set (Vertex × Vertex)
allEdges (GraphImpl obj) = let out = (map adjEdges (O.toUnfoldable obj :: Array (String × (Set Vertex)))) in S.unions out

adjEdges :: Tuple String (Set Vertex) -> Set (Vertex × Vertex)
adjEdges (Tuple id αs) = adjEdges' (Vertex id) αs

adjEdges' :: Vertex -> Set Vertex -> Set (Vertex × Vertex)
adjEdges' v αs = S.map (\node -> (v × node)) αs

reverseEdges :: Endo (Set (Vertex × Vertex))
reverseEdges edges = S.map swap edges

fromEdges :: Set (Vertex × Vertex) -> GraphImpl
fromEdges edges = GraphImpl $
   O.fromFoldableWith S.union (S.map (\pair -> (unwrap (fst pair)) × (S.singleton $ snd pair)) edges)

-- instance Foldable Graph where
--     foldl f z (Graph o) = Graph (foldl f z (map fst (O.values o) :: ?_))
--     foldr f z (Graph o) = Graph (foldr f z o)
--     foldMap f (Graph o) = Graph (foldMap f o)

derive instance Eq Vertex
derive instance Ord Vertex