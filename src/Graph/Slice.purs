module Graph.Slice where

import Data.Foldable (class Foldable)
import Prelude hiding (add)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, lookup, delete, insertWith)
import Data.Map (empty) as M
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Expr (Expr)
import Graph (class Graph, Edge, Vertex, add, addOut, discreteG, elem, inEdges, inEdges', outEdges, outEdges', outN)
import Set (class Set, singleton, empty, unions, member, union)
import Util (type (×), (×))
import Val (Env)

type PendingSlice s = Map Vertex (s Vertex)

bwdSlice :: forall g s. Set s Vertex => Graph g s => s Vertex -> g -> g
bwdSlice αs g' = bwdEdges g' (discreteG αs) (outEdges g' αs)

bwdEdges :: forall g s. Graph g s => g -> g -> List Edge -> g
bwdEdges _ g Nil = g
bwdEdges g' g ((α × β) : es) =
   bwdEdges g' (addOut α β g) $
      es <> if elem β g then Nil else L.fromFoldable (outEdges' g' β)

{-
bwdVertices :: forall g s. Graph g s => g -> g -> List Vertex -> g
bwdVertices _ g Nil = g
bwdVertices g' g (α : αs) = 
   if α `elem` g 
   then ?_
   else ?_
-}
fwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
fwdSlice αs g' = fst $ fwdEdges g' (discreteG αs) M.empty (inEdges g' αs)

fwdEdges :: forall g s. Graph g s => g -> g -> PendingSlice s -> List Edge -> g × (PendingSlice s)
fwdEdges g' g h ((α × β) : es) = fwdEdges g' g'' h' es
   where
   g'' × h' = fwdVertex g' g (insertWith union α (singleton β) h) α
fwdEdges _ currSlice pending Nil = currSlice × pending

fwdVertex :: forall g s. Set s Vertex => Graph g s => g -> g -> PendingSlice s -> Vertex -> g × (PendingSlice s)
fwdVertex g' g h α =
   if αs == outN g' α then
      fwdEdges g' (add α αs g) (delete α h) (inEdges' g' α)
   else g × h
   where
   αs = case lookup α h of
      Just αs' -> αs'
      Nothing -> empty

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
