module Graph.Slice where

import Data.Foldable (class Foldable)
import Prelude hiding (add)
import Data.List (List(..), (:))
import Data.List as L
import Data.Traversable (foldl)
import Data.Tuple (fst)
import Expr (Expr)
import Graph (class Graph, Edge, Vertex, add, addIn, addOut, discreteG, elem, inEdges, inEdges', outEdges, outEdges', outN, remove)
import Set (class Set, singleton, empty, union, unions, member)
import Util (type (×), (×))
import Val (Val, Env)

bwdSlice :: forall g s. Set s Vertex => Graph g s => s Vertex -> g -> g
bwdSlice αs g' = bwdEdges g' (discreteG αs) (outEdges g' αs)

bwdEdges :: forall g s. Graph g s => g -> g -> List Edge -> g
bwdEdges g' g ((α × β) : es) =
   bwdEdges g' (addOut α β g) $
      es <> if elem g β then Nil else L.fromFoldable (outEdges' g' β)
bwdEdges _ g Nil = g

fwdSlice :: forall g s. Graph g s => s Vertex -> g -> g
fwdSlice αs g' = fst $ fwdEdges g' (discreteG αs) mempty (inEdges g' αs)

fwdEdges :: forall g s. Graph g s => g -> g -> g -> List Edge -> g × g
fwdEdges g' g h ((α × β) : es) = fwdEdges g' g'' h' es
   where
   g'' × h' = fwdVertex g' g (addIn α β h) α
fwdEdges _ currSlice pending Nil = currSlice × pending

fwdVertex :: forall g s. Set s Vertex => Graph g s => g -> g -> g -> Vertex -> g × g
fwdVertex g' g h α =
   if αs == outN g' α then
      fwdEdges g' (add α αs g) (remove α h) (inEdges' g' α)
   else g × h
   where
   αs = outN h α

selectVertices :: forall s. Set s Vertex => Val Vertex -> Val Boolean -> s Vertex
selectVertices vα v𝔹 = αs_v
   where
   αs_v = unions (asSet <$> v𝔹 <*> vα)

{-
selectVertices' :: forall s. Set s Vertex => Env Vertex × Expr Vertex -> Env Boolean × Expr Boolean -> s Vertex
selectVertices' (γα × eα) (γ𝔹 × e𝔹) = union αs_e αs_γ
   where
   αs_e = gather (asSet <$> e𝔹 <*> eα)
   αs_γ = gather (gather <$> D.values (D.lift2 asSet γ𝔹 γα) :: List (s Vertex))
-}

selectSourcesFrom :: forall s f. Set s Vertex => Functor f => f Vertex -> s Vertex -> f Boolean
selectSourcesFrom vα αs = v𝔹
   where
   v𝔹 = map (flip member αs) vα

selectSinksFrom :: forall s. Set s Vertex => Env Vertex × Expr Vertex -> s Vertex -> Env Boolean × Expr Boolean
selectSinksFrom (γα × eα) αs = γ𝔹 × e𝔹
   where
   γ𝔹 = map (flip selectSourcesFrom αs) γα
   e𝔹 = selectSourcesFrom eα αs

asSet :: forall s. Set s Vertex => Boolean -> Vertex -> s Vertex
asSet true = singleton
asSet false = const empty
