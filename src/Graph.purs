module Graph where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set (empty, map, singleton, union, unions) as S
import Data.Tuple (Tuple(..), swap)
import Foreign.Object (Object, delete, lookup, singleton, size, toUnfoldable, unionWith) as O
import Util (Endo, (×), type (×))

class Graph g where
   union :: Vertex -> Set Vertex -> Endo g
   outN :: g -> Vertex -> Maybe (Set Vertex)
   inN  :: g -> Vertex -> Maybe (Set Vertex)
   singleton :: Vertex -> Set Vertex -> g
   remove :: Vertex -> Endo g
   opp :: Endo g
   allocate :: g -> Vertex

newtype Vertex = Vertex String

unwrap :: Vertex -> String
unwrap (Vertex string) = string

newtype AnnGraph = AnnGraph (O.Object (Vertex × (Set Vertex)))

newtype GraphImpl = GraphImpl ((O.Object (Set Vertex)) × (O.Object (Set Vertex)))

instance Graph GraphImpl where
   allocate (GraphImpl (obj × _)) = Vertex α 
      where
      α = show $ 1 + (O.size obj)
   remove (Vertex α) (GraphImpl (obj1 × obj2)) = GraphImpl ((O.delete α obj1) × (O.delete α obj2))

   union α αs (GraphImpl (obj1 × obj2)) = (GraphImpl (newObj1 × newObj2))
      where
      newObj1 = O.unionWith S.union obj1 (outStar α αs)
      newObj2 = O.unionWith S.union obj2 (inStar α αs)

   outN (GraphImpl (obj × _)) (Vertex α) = O.lookup α obj
   inN  (GraphImpl (_ × obj)) (Vertex α) = O.lookup α obj

   singleton α αs = GraphImpl (outStar α αs × inStar α αs)
   opp (GraphImpl (obj1 × obj2)) = GraphImpl (obj2 × obj1)

-- these use folds to construct objects of the appropriate fields, with inverted entries, hence the mirror of the structure in each
outStar :: Vertex -> Set Vertex -> O.Object (Set Vertex)
outStar (Vertex α) αs = foldl (O.unionWith S.union) (O.singleton α αs) (S.map (\(Vertex α') -> O.singleton α' S.empty) αs)

inStar :: Vertex -> Set Vertex -> O.Object (Set Vertex)
inStar (Vertex α) αs = foldl (O.unionWith S.union) (O.singleton α S.empty) (S.map (\(Vertex α') -> O.singleton α' (S.singleton (Vertex α))) αs)

allEdges :: GraphImpl -> Set (Vertex × Vertex)
allEdges (GraphImpl (obj × _)) = let out = (map adjEdges (O.toUnfoldable obj :: Array (String × (Set Vertex)))) in S.unions out

adjEdges :: Tuple String (Set Vertex) -> Set (Vertex × Vertex)
adjEdges (Tuple id αs) = adjEdges' (Vertex id) αs

adjEdges' :: Vertex -> Set Vertex -> Set (Vertex × Vertex)
adjEdges' v αs = S.map (\node -> (v × node)) αs

reverseEdges :: Endo (Set (Vertex × Vertex))
reverseEdges edges = S.map swap edges

-- fromEdges :: Set (Vertex × Vertex) -> GraphImpl
-- fromEdges edges = GraphImpl $
--    O.fromFoldableWith S.union (S.map (\pair -> (unwrap (fst pair)) × (S.singleton $ snd pair)) edges)

-- instance Foldable Graph where
--     foldl f z (Graph o) = Graph (foldl f z (map fst (O.values o) :: ?_))
--     foldr f z (Graph o) = Graph (foldr f z o)
--     foldMap f (Graph o) = Graph (foldMap f o)

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _
