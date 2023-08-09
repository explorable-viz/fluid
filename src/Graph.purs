module Graph where

import Prelude

import Control.Monad.State (class MonadState, State, StateT, get, put, runState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.List (List)
import Data.List (fromFoldable, filter, elem, concat) as L
import Data.Maybe (isJust, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (first, second)
import Data.Set (Set)
import Data.Set (delete, empty, map, singleton, union) as S
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst)
import Dict (Dict, delete, empty, fromFoldable, insertWith, lookup, size, unionWith) as D
import Util (Endo, MayFailT, (×), type (×), error)

type Edge = Vertex × Vertex

-- Graphs form a semigroup but we don't actually rely on that (for efficiency).
class Monoid g <= Graph g where
   extend :: Vertex -> Set Vertex -> Endo g
   elem :: g -> Vertex -> Boolean
   outN :: g -> Vertex -> Set Vertex
   inN :: g -> Vertex -> Set Vertex
   size :: g -> Int
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

data GraphAccum2T g m a = GraphAccum2T (g -> m (a × g))
type WithGraph2 g a = MayFailT (GraphAccum2T g (State Int)) a

runGraphAccumT :: forall g m a. GraphAccumT g m a -> m (a × Endo g)
runGraphAccumT (GraphAccumT m) = m

runGraphAccum2T :: forall g m a. GraphAccum2T g m a -> g -> m (a × g)
runGraphAccum2T (GraphAccum2T m) = m

instance Functor m => Functor (GraphAccumT g m) where
   map f (GraphAccumT m) = GraphAccumT $ m <#> first f

instance Functor m => Functor (GraphAccum2T g m) where
   map f (GraphAccum2T m) = GraphAccum2T $ \g -> m g <#> first f

instance Apply m => Apply (GraphAccumT g m) where
   apply (GraphAccumT m) (GraphAccumT m') = GraphAccumT $ k <$> m <*> m'
      where
      k (f × g) (x × g') = f x × (g >>> g')

instance (Apply m, Monad m) => Apply (GraphAccum2T g m) where
   apply = ap

instance Bind m => Bind (GraphAccumT g m) where
   bind (GraphAccumT m) f = GraphAccumT $ do
      x × g <- m
      let GraphAccumT m' = f x
      m' <#> second ((>>>) g)

instance Monad m => Bind (GraphAccum2T g m) where
   bind (GraphAccum2T x) f = GraphAccum2T \g ->
      x g >>= \(y × g') -> case f y of GraphAccum2T x' -> x' g'

instance (Monoid g, Applicative m) => Applicative (GraphAccumT g m) where
   pure a = GraphAccumT $ pure $ a × identity

instance Monad m => Applicative (GraphAccum2T g m) where
   pure x = GraphAccum2T \g -> pure $ x × g

instance (Monoid g, Monad m) => Monad (GraphAccumT g m)

instance Monad m => Monad (GraphAccum2T g m)

instance Monoid g => MonadTrans (GraphAccumT g) where
   lift m = GraphAccumT $ (×) <$> m <@> identity

instance Monoid g => MonadTrans (GraphAccum2T g) where
   lift m = GraphAccum2T \g -> (×) <$> m <@> g

instance (Graph g, MonadAlloc m) => MonadGraphAccum g (GraphAccumT g m) where
   new αs = do
      α <- lift $ fresh
      GraphAccumT $ pure $ α × extend α αs

instance (Graph g, MonadAlloc m) => MonadGraphAccum g (GraphAccum2T g m) where
   new αs = do
      α <- lift $ fresh
      GraphAccum2T $ \g -> pure $ α × extend α αs g

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

   extend (Vertex α) αs (GraphImpl out in_) = GraphImpl newOut newIn
      where
      newOut = foldl (\d (Vertex α') -> D.insertWith S.union α' S.empty d) (D.insertWith S.union α αs out) αs
      newIn = foldl (\d (Vertex α') -> D.insertWith S.union α' (S.singleton (Vertex α)) d) (D.insertWith S.union α S.empty in_) αs

   outN (GraphImpl out _) (Vertex α) = case D.lookup α out of
      Just αs -> αs
      Nothing -> error "not in graph"
   inN (GraphImpl _ in_) (Vertex α) = case D.lookup α in_ of
      Just αs -> αs
      Nothing -> error ("Looked up " <> α <> " in " <> show in_)

   elem (GraphImpl out _) (Vertex α) = isJust (D.lookup α out)
   size (GraphImpl out _) = D.size out

   opp (GraphImpl out in_) = GraphImpl in_ out

   discreteG αs = GraphImpl discreteM discreteM
      where
      pairs = S.map (\(Vertex α) -> α × S.empty) αs
      discreteM = D.fromFoldable pairs

instance Show GraphImpl where
   show (GraphImpl out in_) = "GraphImpl (" <> show out <> " × " <> show in_ <> ")"
