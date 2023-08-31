module Graph.Slice where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, lookup, delete, insertWith)
import Data.Map (empty) as M
import Data.Set (Set)
import Data.Tuple (fst)
import Graph (class Graph, Edge, Vertex, inEdges, inEdges', outN, sinks, op)
import Graph.GraphWriter (WithGraph, extend, runWithGraph)
import Set (class Set, empty, insert, member, singleton, union, unions, difference)
import Util (type (×), (×), definitely)

type PendingSlice s = Map Vertex (s Vertex)

bwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
bwdSlice αs' g' = fst $ runWithGraph $ tailRecM go (empty × L.fromFoldable αs')
   where
   go :: (s Vertex × List Vertex) -> WithGraph s (Step (s Vertex × List Vertex) Unit)
   go (_ × Nil) = pure $ Done unit
   go (visited × (α : αs)) = do
      let βs = outN g' α
      extend α βs
      pure $ Loop ((visited # insert α) × (L.fromFoldable βs <> αs))

fwdSliceDeMorgan :: forall g s. Graph g s => s Vertex -> g -> g
fwdSliceDeMorgan αs g' =
   bwdSlice (sinks g' `difference` αs) (op g')

fwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
fwdSlice αs g' =
   fst $ runWithGraph $ fwdEdges g' M.empty (inEdges g' αs)

fwdEdges :: forall g s. Graph g s => g -> PendingSlice s -> List Edge -> WithGraph s (PendingSlice s)
fwdEdges _ pending Nil = pure pending
fwdEdges g' h ((α × β) : es) = do
   let hαβ = insertWith union α (singleton β) h
       βs = lookup α hαβ # definitely "in pending map"
   if βs == outN g' α
      then do extend α βs
              fwdEdges g' (delete α hαβ) (inEdges' g' α <> es)
      else fwdEdges g' hαβ es

selectαs :: forall f. Apply f => Foldable f => f Boolean -> f Vertex -> Set Vertex
selectαs v𝔹 vα = unions (asSet <$> v𝔹 <*> vα)

select𝔹s :: forall f. Functor f => f Vertex -> Set Vertex -> f Boolean
select𝔹s vα αs = flip member αs <$> vα

asSet :: forall s. Set s Vertex => Boolean -> Vertex -> s Vertex
asSet true = singleton
asSet false = const empty

-- fwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
-- fwdSlice αs' g' = fst $ runWithGraph $ tailRecM go (M.empty × inEdges g' αs') where
--    go :: (PendingSlice s × List Edge) -> WithGraph s (Step (PendingSlice s × List Edge) (PendingSlice s))
--    go (h × Nil) = pure $ Done h
--    go (h × ((α × β) : es)) = do
--       fwdVertex (insertWith union α (singleton β) h) α >>= case _ of
--          Done h' -> pure $ Loop (h' × es)
--          Loop (h' × es') -> do _ <- pure $ Loop (h' × es')
--                                pure $ Loop (delete α h × inEdges' g' α)
--    fwdVertex ::PendingSlice s -> Vertex -> WithGraph s (Step (PendingSlice s × List Edge) (PendingSlice s))
--    fwdVertex h α =
--       if αs == outN g' α then do
--          extend α αs
--          pure $ Loop (delete α h × inEdges' g' α)
--       else pure $ Done h
--       where
--       αs = lookup α h # definitely "in pending map"
