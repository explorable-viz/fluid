module DependenceGraph where

import Prelude

import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (empty, singleton, union) as S
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object, delete, fromFoldable, insert, keys, lookup, singleton, size, unionWith, values) as O
import Util (Endo, error)


class Graph g where
    union     :: Vertex -> Set Vertex -> Endo g
    outN      :: g -> Vertex -> Set Vertex
    singleton :: Vertex -> Set Vertex -> g
    remove    :: Vertex -> Endo g
    opp       :: Endo g
    allocate  :: g -> Vertex

newtype Vertex = Vertex String

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
                                            Nothing    -> S.empty
    singleton (Vertex key) neighbs = SimpleGraph (O.singleton key neighbs)
    opp (SimpleGraph _) = error "todo"
                        -- SimpleGraph $ foldrWithIndex combine obj verts
                        --   where
                        --   combine v edges = O.unionWith S.union (O.fromFoldable edges)
                        --   verts = O.keys obj

-- instance Foldable Graph where
--     foldl f z (Graph o) = Graph (foldl f z (map fst (O.values o) :: ?_))
--     foldr f z (Graph o) = Graph (foldr f z o)
--     foldMap f (Graph o) = Graph (foldMap f o)
