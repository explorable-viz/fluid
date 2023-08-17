module Graph.Slice where

import Prelude hiding (add)

import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, lookup, delete, insertWith)
import Data.Map (empty) as M
import Data.Tuple (fst)
import Expr (Expr)
import Graph (class Graph, Edge, Vertex, inEdges, inEdges', outN)
import Graph.GraphWriter (WithGraph2, extend, runWithGraph2)
import Set (class Set, empty, insert, member, singleton, union, unions)
import Util (type (×), (×), definitely)
import Val (Env)

type PendingSlice s = Map Vertex (s Vertex)

bwdSlice :: forall g s. Set s Vertex => Graph g s => s Vertex -> g -> g
bwdSlice αs g' =
   fst $ runWithGraph2 $ bwdVertices g' empty (L.fromFoldable αs)

bwdVertices :: forall g s. Graph g s => g -> s Vertex -> List Vertex -> WithGraph2 s Unit
bwdVertices _ _ Nil = pure unit
bwdVertices g' visited (α : αs) =
   if α `member` visited then bwdVertices g' visited αs
   else do
      let βs = outN g' α
      extend α βs
      bwdVertices g' (visited # insert α) (L.fromFoldable βs <> αs)

fwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
fwdSlice αs g' =
   fst $ runWithGraph2 $ fwdEdges g' M.empty (inEdges g' αs)

fwdEdges :: forall g s. Graph g s => g -> PendingSlice s -> List Edge -> WithGraph2 s (PendingSlice s)
fwdEdges _ pending Nil = pure pending
fwdEdges g' h ((α × β) : es) = do
   h' <- fwdVertex g' (insertWith union α (singleton β) h) α
   fwdEdges g' h' es

fwdVertex :: forall g s. Set s Vertex => Graph g s => g -> PendingSlice s -> Vertex -> WithGraph2 s (PendingSlice s)
fwdVertex g' h α =
   if αs == outN g' α then do
      extend α αs
      fwdEdges g' (delete α h) (inEdges' g' α)
   else pure h
   where
   αs = lookup α h # definitely "in pending map"

selectVertices :: forall s f. Set s Vertex => Apply f => Foldable f => f Vertex -> f Boolean -> s Vertex
selectVertices vα v𝔹 = αs_v
   where
   αs_v = unions (asSet <$> v𝔹 <*> vα)

select𝔹s :: forall s f. Set s Vertex => Functor f => f Vertex -> s Vertex -> f Boolean
select𝔹s vα αs = v𝔹
   where
   v𝔹 = map (flip member αs) vα

select𝔹s' :: forall s. Set s Vertex => Env Vertex × Expr Vertex -> s Vertex -> Env Boolean × Expr Boolean
select𝔹s' (γα × eα) αs = γ𝔹 × e𝔹
   where
   γ𝔹 = map (flip select𝔹s αs) γα
   e𝔹 = select𝔹s eα αs

asSet :: forall s. Set s Vertex => Boolean -> Vertex -> s Vertex
asSet true = singleton
asSet false = const empty
