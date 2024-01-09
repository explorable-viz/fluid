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
import Util (type (×), singleton, spyWith, (×), (∈))

type BwdConfig =
   { visited :: Set Vertex
   , αs :: List Vertex
   , pending :: List HyperEdge
   }

{-
bwdSlice :: forall g. Graph g => Set Vertex × g -> g
bwdSlice (αs × g) =
   fst (runWithGraph_spy (tailRecM go { visited: empty, αs: L.fromFoldable αs }) Fwd empty)
   where
   go :: BwdConfig -> WithGraph (Step BwdConfig Unit)
   go { αs: Nil } = pure $ Done unit
   go { visited, αs: α : αs' } =
      if α ∈ visited then
         pure $ Loop { visited, αs: αs' }
      else do
         let βs = outN g α
         extend α βs
         pure $ Loop { visited: insert α visited, αs: L.fromFoldable βs <> αs' }
-}
bwdSlice :: forall g. Graph g => Set Vertex × g -> g
bwdSlice (αs × g) = fst (runWithGraph_spy (go { visited: empty, αs: L.fromFoldable αs, pending: Nil }) Fwd empty)
   where
   go :: BwdConfig -> WithGraph Unit
   go { αs: Nil, pending: Nil } = pure unit
   go { visited, αs: Nil, pending: (α × βs) : pending } = do
      if (spyWith "popping" show α ∈ visited) then
         go { visited, αs: Nil, pending }
      else do
         extend (spyWith "adding" show α) βs
         go { visited: insert α visited, αs: Nil, pending }
   go { visited, αs: α : αs', pending } = do
      let βs = outN g (spyWith "pushing" show α)
      go { visited, αs: L.fromFoldable βs <> αs', pending: (α × βs) : pending }

type PendingVertices = Map Vertex (Set Vertex)
type FwdConfig =
   { pending :: PendingVertices
   , es :: List Edge
   }

fwdSlice :: forall g. Graph g => Set Vertex × g -> g
fwdSlice (αs × g) =
   fst (runWithGraph_spy (tailRecM go { pending: M.empty, es: inEdges g αs }) Fwd αs)
   where
   go :: FwdConfig -> WithGraph (Step FwdConfig Unit)
   go { es: Nil } = pure $ Done unit
   go { pending, es: (α × β) : es } =
      if βs == outN g α then do
         extend α βs
         pure $ Loop { pending: M.delete α pending, es: inEdges' g α <> es }
      else
         pure $ Loop { pending: M.insert α βs pending, es }
      where
      βs = maybe (singleton β) (insert β) (lookup α pending)
