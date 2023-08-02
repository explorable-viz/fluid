module Graph where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set (delete, empty, map, singleton, union) as S
import Foreign.Object (Object, delete, fromFoldable, lookup, singleton, size, unionWith) as SM
import Util (Endo, (×), type (×))

type SMap = SM.Object

class Graph g where
   union :: Vertex -> Set Vertex -> Endo g
   outN :: g -> Vertex -> Maybe (Set Vertex)
   inN :: g -> Vertex -> Maybe (Set Vertex)
   singleton :: Vertex -> Set Vertex -> g
   remove :: Vertex -> Endo g
   opp :: Endo g
   allocate :: g -> Vertex

newtype Vertex = Vertex String

unwrap :: Vertex -> String
unwrap (Vertex string) = string

newtype AnnGraph = AnnGraph (SMap (Vertex × (Set Vertex)))

newtype GraphImpl = GraphImpl ((SMap (Set Vertex)) × (SMap (Set Vertex)))

instance Graph GraphImpl where
   allocate (GraphImpl (in_ × _)) = Vertex α
      where
      α = show $ 1 + (SM.size in_)
   remove (Vertex α) (GraphImpl (out × in_)) =
      let
         newOutN = map (S.delete (Vertex α)) (SM.delete α out)
         newInN = map (S.delete (Vertex α)) (SM.delete α in_)
      in
         GraphImpl (newOutN × newInN)
   union α αs (GraphImpl (out × in_)) = (GraphImpl (newOut × newIn))
      where
      newOut = SM.unionWith S.union out (outStar α αs)
      newIn = SM.unionWith S.union in_ (inStar α αs)

   outN (GraphImpl (out × _)) (Vertex α) = SM.lookup α out
   inN (GraphImpl (_ × in_)) (Vertex α) = SM.lookup α in_

   singleton α αs = GraphImpl (outStar α αs × inStar α αs)
   opp (GraphImpl (outN × inN)) = GraphImpl (inN × outN)

-- Initial attempts at making stargraphs, using foldl to construct intermediate objects
outStarOld :: Vertex -> Set Vertex -> SMap (Set Vertex)
outStarOld (Vertex α) αs = foldl (SM.unionWith S.union) (SM.singleton α αs) (S.map (\(Vertex α') -> SM.singleton α' S.empty) αs)

inStarOld :: Vertex -> Set Vertex -> SMap (Set Vertex)
inStarOld (Vertex α) αs = foldl (SM.unionWith S.union) (SM.singleton α S.empty) (S.map (\(Vertex α') -> SM.singleton α' (S.singleton (Vertex α))) αs)

-- prototype attempts at more efficiently implementing the above operations
outStar :: Vertex -> Set Vertex -> SMap (Set Vertex)
outStar v@(Vertex α) αs = SM.unionWith S.union (SM.singleton α αs) (star' v αs)

star' :: Vertex -> Set Vertex -> SMap (Set Vertex)
star' (Vertex α) αs = SM.fromFoldable $ S.map (\α' -> α × (S.singleton α')) αs

star'' :: Vertex -> Set Vertex -> SMap (Set Vertex)
star'' α αs = SM.fromFoldable $ S.map (\(Vertex α') -> α' × (S.singleton α)) αs

inStar :: Vertex -> Set Vertex -> SMap (Set Vertex)
inStar v@(Vertex α) αs = SM.unionWith S.union (SM.singleton α S.empty) (star'' v αs)

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _
