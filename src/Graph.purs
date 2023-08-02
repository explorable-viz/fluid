module Graph where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set (delete, difference, empty, fromFoldable, map, member, singleton, subset, union, unions, filter) as S
import Foreign.Object (Object, delete, empty, filterKeys, fromFoldable, keys, lookup, singleton, size, unionWith) as SM
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

newtype AnnGraph = AnnGraph (SMap (Vertex × (Set Vertex)))

newtype GraphImpl = GraphImpl ((SMap (Set Vertex)) × (SMap (Set Vertex)))

instance Graph GraphImpl where
   allocate (GraphImpl (out × _)) = Vertex α
      where
      α = show $ 1 + (SM.size out)
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

emptyG :: GraphImpl
emptyG = GraphImpl (SM.empty × SM.empty)

subgraph :: GraphImpl -> Set Vertex -> GraphImpl
subgraph (GraphImpl (out × in_)) αs =
   let
      keys = S.fromFoldable $ SM.keys out
      αNames = S.map unwrap αs
   in
      if S.subset αNames keys then
         let
            αs' = S.map Vertex (S.difference keys αNames)
            filteredOut = SM.filterKeys (\k -> S.member k αNames) out
            filteredIn = SM.filterKeys (\k -> S.member k αNames) in_
            newOut = map (S.difference αs') filteredOut
            newIn = map (S.difference αs') filteredIn
         in
            GraphImpl (newOut × newIn)
      else
         emptyG

outE' :: forall g. Graph g => g -> Vertex -> Set (Vertex × Vertex)
outE' graph α = case outN graph α of
   Just set -> S.map (\node -> α × node) set
   Nothing -> S.empty

outE :: Set Vertex -> GraphImpl -> Set (Vertex × Vertex)
outE αs g =
   let
      allOut = S.unions (S.map (\α -> outE' g α) αs)
   in
      S.filter (\(e1 × e2) -> not $ S.member e1 αs || S.member e2 αs) allOut

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

elem :: GraphImpl -> Vertex -> Boolean
elem (GraphImpl (out × _)) (Vertex α) =
   case SM.lookup α out of
      Just _ -> true
      Nothing -> false

-- bwdSlice :: Set Vertex -> GraphImpl -> GraphImpl
-- bwdSlice αs parent = bwdSlice' parent startG edges
--    where
--    startG = error "todo"
--    edges = error "todo"

-- bwdSlice' :: GraphImpl -> GraphImpl -> Set (Vertex × Vertex) -> GraphImpl
-- bwdSlice' _parent g s =
--    if S.isEmpty s then g
--    else emptyG

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _
