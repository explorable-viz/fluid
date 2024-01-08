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
import Graph (class Graph, Direction(..), Edge, Vertex, HyperEdge, inEdges, inEdges', outN)
import Graph.WithGraph (WithGraph, extend, runWithGraph_spy)
import Util (type (×), singleton, (×), (∈))

type BwdConfig =
   { visited :: Set Vertex
   , αs :: List Vertex
   , pending :: List HyperEdge
   }

bwdSlice :: forall g. Graph g => Set Vertex × g -> g
bwdSlice (αs × g) =
   fst (runWithGraph_spy (tailRecM go { visited: empty, αs: L.fromFoldable αs, pending: Nil }) Bwd αs)
   where
   go :: BwdConfig -> WithGraph (Step BwdConfig Unit)
   go { αs: Nil, pending: Nil } = pure $ Done unit
   go { visited, αs: Nil, pending: (α × βs) : pending } = do
      extend α βs
      pure $ Loop { visited, αs: Nil, pending }
   go { visited, αs: α : αs', pending } =
      if α ∈ visited then
         pure $ Loop { visited, αs: αs', pending }
      else do
         let βs = outN g α
         extend α βs
         pure $ Loop { visited: insert α visited, αs: L.fromFoldable βs <> αs', pending }

type PendingVertices = Map Vertex (Set Vertex)
type FwdConfig =
   { pending :: PendingVertices
   , es :: List Edge
   }

fwdSlice :: forall g. Graph g => Set Vertex × g -> g
fwdSlice (αs × g) =
   fst (runWithGraph_spy (tailRecM go { pending: M.empty, es: inEdges g αs }) Fwd αs)
   where
   go :: FwdConfig -> WithGraph (Step FwdConfig PendingVertices)
   go { pending: h, es: Nil } = Done <$> pure h
   go { pending: h, es: (α × β) : es } = Loop <$>
      if βs == outN g α then do
         extend α βs
         pure { pending: M.delete α h, es: inEdges' g α <> es }
      else
         pure { pending: M.insert α βs h, es }
      where
      βs = maybe (singleton β) (insert β) (lookup α h)
