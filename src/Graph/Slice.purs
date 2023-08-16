module Graph.Slice where

import Data.Foldable (class Foldable, foldl)
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
import Val (Env, Val(..), Fun(..))

type PendingSlice s = Map Vertex (s Vertex)

bwdSlice :: forall g s. Set s Vertex => Graph g s => s Vertex -> g -> g
bwdSlice αs g' = bwdEdges g' (discreteG αs) (outEdges g' αs)

bwdEdges :: forall g s. Graph g s => g -> g -> List Edge -> g
bwdEdges g' g ((α × β) : es) =
   bwdEdges g' (addOut α β g) $
      es <> if elem g β then Nil else L.fromFoldable (outEdges' g' β)
bwdEdges _ g Nil = g

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

allVertices :: forall s f. Set s Vertex => Apply f => Foldable f => f Vertex -> s Vertex
allVertices vα = selectVertices vα v𝔹
   where
   v𝔹 = map (const true) vα

envVertices :: forall s. Set s Vertex => Env Vertex -> s Vertex
envVertices env = foldl (\set val -> union (getVertex val) set) empty env
   where
   getVertex :: Val Vertex -> s Vertex
   getVertex (Fun (Closure α _ _ _)) = singleton α
   getVertex (Fun (PartialConstr α _ _)) = singleton α
   getVertex (Int α _) = singleton α
   getVertex (Float α _) = singleton α
   getVertex (Str α _) = singleton α
   getVertex (Constr α _ _) = singleton α
   getVertex (Record α _) = singleton α
   getVertex (Dictionary α _) = singleton α
   getVertex (Matrix α _) = singleton α
   getVertex _ = empty

{-
selectVertices' :: forall s. Set s Vertex => Env Vertex × Expr Vertex -> Env Boolean × Expr Boolean -> s Vertex
selectVertices' (γα × eα) (γ𝔹 × e𝔹) = union αs_e αs_γ
   where
   αs_e = gather (asSet <$> e𝔹 <*> eα)
   αs_γ = gather (gather <$> D.values (D.lift2 asSet γ𝔹 γα) :: List (s Vertex))
-}

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
