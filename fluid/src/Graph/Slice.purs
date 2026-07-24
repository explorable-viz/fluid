module Graph.Slice where

import Prelude hiding (map)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List (List(..), (:))
import Data.List as L
import Data.Set (Set, empty, insert)
import Data.Tuple (fst)
import Graph (class Graph, DVertex'(..), HyperEdge, Vertex, addresses, outN, vertexData)
import Graph.WithGraph (WithGraph, extend, runWithGraph_spy)
import Test.Util.Debug (checking)
import Util (type (×), validateWhen, (×), (⊆))
import Util.Set ((∈))

type BwdConfig =
   { visited :: Set Vertex
   , αs :: List Vertex
   , pending :: List HyperEdge
   }

bwdSlice :: forall g. Graph g => Set Vertex × g -> g
bwdSlice (αs × g) = fst $
   αs
      -- No outputsAreSources analog of inputAreSinks; we do however need to restrict to sources (see #818).
      # validateWhen checking.outputsInGraph "inputs are sinks" (_ ⊆ addresses g)
      -- # (\_ -> αs ∩ sources g) 
      # \αs' -> runWithGraph_spy (tailRecM go { visited: empty, αs: L.fromFoldable αs', pending: Nil }) empty
   where

   go :: BwdConfig -> WithGraph (Step BwdConfig Unit)
   go { αs: Nil, pending: Nil } = pure $ Done unit
   go { visited, αs: Nil, pending: (DVertex (α × vd) × βs) : pending } = do
      if α ∈ visited then
         pure $ Loop { visited, αs: Nil, pending }
      else do
         extend (DVertex (α × vd)) βs
         pure $ Loop { visited: insert α visited, αs: Nil, pending }
   go { visited, αs: α : αs', pending } = do
      let βs = outN g α
      pure $ Loop { visited, αs: L.fromFoldable βs <> αs', pending: (DVertex (α × vertexData g α) × βs) : pending }
