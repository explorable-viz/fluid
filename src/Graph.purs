module Graph where

import Prelude

import Control.Monad.State (class MonadState, State, StateT, get, put, runState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List (fromFoldable, filter, elem, concat) as L
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (first, second)
import Data.Set (Set)
import Data.Set (delete, empty, map, singleton, union) as S
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst)
import Dict (Dict, delete, empty, fromFoldable, lookup, singleton, size, unionWith) as D
import Util (Endo, MayFailT, (×), type (×), error)

type Edge = Vertex × Vertex

-- Graphs form a semigroup but we don't actually rely on that (for efficiency).
class Monoid g <= Graph g where
   extend :: Vertex -> Set Vertex -> Endo g
   elem :: g -> Vertex -> Boolean
   outN :: g -> Vertex -> Set Vertex
   inN :: g -> Vertex -> Set Vertex
   singleton :: Vertex -> Set Vertex -> g
   remove :: Vertex -> Endo g
   opp :: Endo g
   discreteG :: Set Vertex -> g

newtype Vertex = Vertex String

type HeapT m a = StateT Int m a
type Heap a = HeapT Identity a

class MonadState Int m <= MonadAlloc m where
   fresh :: m Vertex

instance Monad m => MonadAlloc (StateT Int m) where
   fresh = do
      s <- get
      put (s + 1)
      pure (Vertex (show s))

{-# Allocating addresses #-}
runHeap :: forall a. Heap a -> a
runHeap = flip runState 0 >>> fst

alloc :: forall t a. Traversable t => t a -> Heap (t Vertex)
alloc = traverse (const fresh)

-- Difference graphs
class (Graph g, Monad m) <= MonadGraphAccum g m | m -> g where
   -- Extend graph with fresh vertex pointing to set of existing vertices; return new vertex.
   new :: Set Vertex -> m Vertex

-- Essentially Writer instantiated to a monoid of endofunctions
data GraphAccumT g m a = GraphAccumT (m (a × Endo g))
type WithGraph g a = MayFailT (GraphAccumT g (State Int)) a

runGraphAccumT :: forall g m a. GraphAccumT g m a -> m (a × Endo g)
runGraphAccumT (GraphAccumT m) = m

instance Functor m => Functor (GraphAccumT g m) where
   map f (GraphAccumT m) = GraphAccumT $ m <#> first f

instance Apply m => Apply (GraphAccumT g m) where
   apply (GraphAccumT m) (GraphAccumT m') = GraphAccumT $ k <$> m <*> m'
      where
      k (f × g) (x × g') = f x × (g >>> g')

instance Bind m => Bind (GraphAccumT g m) where
   bind (GraphAccumT m) f = GraphAccumT $ do
      x × g <- m
      let GraphAccumT m' = f x
      m' <#> second ((>>>) g)

instance (Monoid g, Applicative m) => Applicative (GraphAccumT g m) where
   pure a = GraphAccumT $ pure $ a × identity

instance (Monoid g, Monad m) => Monad (GraphAccumT g m)

instance Monoid g => MonadTrans (GraphAccumT g) where
   lift m = GraphAccumT $ (×) <$> m <@> identity

instance (Graph g, MonadAlloc m) => MonadGraphAccum g (GraphAccumT g m) where
   new αs = do
      α <- lift $ fresh
      GraphAccumT $ pure $ α × extend α αs

outE' :: forall g. Graph g => g -> Vertex -> List Edge
outE' graph α = L.fromFoldable $ S.map (α × _) (outN graph α)

outE :: forall g. Graph g => Set Vertex -> g -> List Edge
outE αs g = L.filter (\(e1 × e2) -> L.elem e1 αs || L.elem e2 αs) allOut
   where
   allOut = L.concat (map (\α -> outE' g α) (L.fromFoldable αs))

inE' :: forall g. Graph g => g -> Vertex -> List Edge
inE' graph α = L.fromFoldable $ S.map (_ × α) (inN graph α)

inE :: forall g. Graph g => Set Vertex -> g -> List Edge
inE αs g = L.filter (\(e1 × e2) -> L.elem e1 αs || L.elem e2 αs) allIn
   where
   allIn = L.concat (map (\α -> inE' g α) (L.fromFoldable αs))

bwdSlice :: forall g. Graph g => Set Vertex -> g -> g
bwdSlice αs g' = bwdEdges g' (discreteG αs) (outE αs g')

bwdEdges :: forall g. Graph g => g -> g -> List Edge -> g
bwdEdges g' g ((α × β) : es) =
   if elem g β then
      bwdEdges g' (extend α (S.singleton β) g) es
   else
      bwdEdges g' (extend α (S.singleton β) g) (es <> (L.fromFoldable (outE' g' β)))
bwdEdges _ g Nil = g

fwdSlice :: forall g. Graph g => Set Vertex -> g -> g
fwdSlice αs g' = fst $ fwdEdges g' (discreteG αs) mempty (inE αs g')

fwdEdges :: forall g. Graph g => g -> g -> g -> List Edge -> g × g
fwdEdges g' g h ((α × β) : es) = fwdEdges g' g'' h' es
   where
   (g'' × h') = fwdVertex g' g (extend α (S.singleton β) h) α
fwdEdges _ currSlice pending Nil = currSlice × pending

fwdVertex :: forall g. Graph g => g -> g -> g -> Vertex -> g × g
fwdVertex g' g h α =
   if αs == (outN g' α) then
        fwdEdges g' (extend α αs g) (remove α h) (inE' g' α)
   else g × h
   where
   αs = outN h α

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _

instance Show Vertex where
   show (Vertex α) = "Vertex " <> α

-- GraphImpl Specifics
data GraphImpl = GraphImpl (D.Dict (Set Vertex)) (D.Dict (Set Vertex))

instance Semigroup GraphImpl where
   append (GraphImpl out1 in1) (GraphImpl out2 in2) =
      GraphImpl (D.unionWith S.union out1 out2) (D.unionWith S.union in1 in2)

instance Monoid GraphImpl where
   mempty = GraphImpl D.empty D.empty

empty :: GraphImpl
empty = mempty

instance Graph GraphImpl where
   remove (Vertex α) (GraphImpl out in_) = GraphImpl newOutN newInN
      where
      newOutN = map (S.delete (Vertex α)) (D.delete α out)
      newInN = map (S.delete (Vertex α)) (D.delete α in_)

   extend α αs (GraphImpl out in_) = GraphImpl newOut newIn
      where
      newOut = D.unionWith S.union out (starInOut α αs)
      newIn = D.unionWith S.union in_ (starInIn α αs)

   outN (GraphImpl out _) (Vertex α) = maybe (error "not in graph") identity $ D.lookup α out
   inN (GraphImpl _ in_) (Vertex α) = maybe (error "not in graph") identity $ D.lookup α in_

   elem (GraphImpl out _) (Vertex α) = isJust (D.lookup α out)

   singleton α αs = GraphImpl (starInOut α αs) (starInIn α αs)
 
   opp (GraphImpl out in_) = GraphImpl in_ out

   discreteG αs = GraphImpl discreteM discreteM
      where
      pairs = S.map (\(Vertex α) -> α × S.empty) αs
      discreteM = D.fromFoldable pairs

-- prototype attempts at more efficiently implementing the above operations
starInOut :: Vertex -> Set Vertex -> D.Dict (Set Vertex)
starInOut (Vertex α) αs = D.unionWith S.union (D.singleton α αs) (star αs)
   where
   star :: Set Vertex -> D.Dict (Set Vertex)
   star αs' = D.fromFoldable $ S.map (\(Vertex α') -> α' × S.empty) αs'

starInIn :: Vertex -> Set Vertex -> D.Dict (Set Vertex)
starInIn v@(Vertex α) αs = D.unionWith S.union (D.singleton α S.empty) (star v αs)
   where
   star :: Vertex -> Set Vertex -> D.Dict (Set Vertex)
   star α' αs' = D.fromFoldable $ S.map (\(Vertex α'') -> α'' × (S.singleton α')) αs'

inStar :: Vertex -> Set Vertex -> GraphImpl
inStar α αs = opp (outStar α αs)

outStar :: Vertex -> Set Vertex -> GraphImpl
outStar α αs = GraphImpl (starInOut α αs) (starInIn α αs)

instance Show GraphImpl where
   show (GraphImpl out in_) = "GraphImpl (" <> show out <> " × " <> show in_ <> ")"
