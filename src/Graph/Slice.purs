module Graph.Slice where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (maybe)
import Data.Set (Set, empty, insert)
import Data.Tuple (fst)
import Graph (class Graph, Edge, Vertex, Direction(..), inEdges, inEdges', outN)
import Graph.WithGraph (WithGraph, extend, runWithGraph_spy)
import Util (type (×), singleton, (×), (∈))

type BwdConfig =
   { visited :: Set Vertex
   , αs :: List Vertex
   }

bwdSlice :: forall g. Graph g => Set Vertex × g -> g
bwdSlice (αs × g) =
   fst (runWithGraph_spy (tailRecM go { visited: empty, αs: L.fromFoldable αs }) Bwd αs)
   where
   go :: BwdConfig -> WithGraph (Step BwdConfig Unit)
   go { αs: Nil } = Done <$> pure unit
   go { visited, αs: α : αs' } = Loop <$>
      if α ∈ visited then
         pure { visited, αs: αs' }
      else do
         let βs = outN g α
         extend α βs
         pure { visited: insert α visited, αs: L.fromFoldable βs <> αs' }

type PendingVertices = Map Vertex (Set Vertex)
type FwdConfig = PendingVertices × List Edge

fwdSlice :: forall g. Graph g => Set Vertex × g -> g
fwdSlice (αs × g) =
   fst (runWithGraph_spy (tailRecM go (M.empty × inEdges g αs)) Fwd αs)
   where
   go :: FwdConfig -> WithGraph (Step FwdConfig PendingVertices)
   go (h × Nil) = Done <$> pure h
   go (h × ((α × β) : es)) = Loop <$>
      if βs == outN g α then do
         extend α βs
         pure (M.delete α h × (inEdges' g α <> es))
      else
         pure (M.insert α βs h × es)
      where
      βs = maybe (singleton β) (insert β) (lookup α h)
