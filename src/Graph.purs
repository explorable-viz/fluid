module Graph where

import Prelude

import Control.Monad.State (class MonadState, State, StateT, get, put, runState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT, tell)
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.List (List, (:))
import Data.List (fromFoldable, filter, elem, concat) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (first, second)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst)
import Dict as D
import Util (Endo, MayFailT, (×), type (×), definitely)

type Edge = Vertex × Vertex

-- Graphs form a semigroup but we don't actually rely on that (for efficiency).
class Monoid g <= Graph g where
   extend :: Vertex -> Set Vertex -> Endo g
   -- connectOut α β adds β as new out-neighbour of existing vertex α, adding into g if necessary
   connectOut :: Vertex -> Vertex -> Endo g
   -- connectIn α β adds α as new in-neighbour of existing vertex β, adding into g if necessary
   -- connectIn α β G = op (connectOut β α (op G)
   connectIn :: Vertex -> Vertex -> Endo g
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
class Monad m <= MonadGraphAccum m where
   -- Extend graph with fresh vertex pointing to set of existing vertices; return new vertex.
   new :: Set Vertex -> m Vertex

-- Essentially Writer instantiated to a monoid of endofunctions
data GraphAccumT g m a = GraphAccumT (m (a × Endo g))
type WithGraph g a = MayFailT (GraphAccumT g (State Int)) a

data GraphAccum2T g m a = GraphAccum2T (g -> m (a × g))
type WithGraph2 g a = MayFailT (GraphAccum2T g (State Int)) a

type GraphExtension = List (Vertex × Set Vertex) -- list of successive arguments to `extend`
type WithGraph3 a = MayFailT (WriterT (Endo GraphExtension) (State Int)) a

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

instance (Graph g, MonadAlloc m) => MonadGraphAccum (GraphAccumT g m) where
   new αs = do
      α <- lift $ fresh
      GraphAccumT $ pure $ α × extend α αs

instance (Graph g, MonadAlloc m) => MonadGraphAccum (GraphAccum2T g m) where
   new αs = do
      α <- lift $ fresh
      GraphAccum2T $ \g -> pure $ α × extend α αs g

instance MonadAlloc m => MonadGraphAccum (MayFailT (WriterT (Endo GraphExtension) m)) where
   new αs = do
      α <- lift $ lift $ fresh
      tell $ (:) (α × αs)
      pure α

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

-- Maintain out neighbours and in neighbours as separate adjacency maps with a common domain.
data GraphImpl = GraphImpl (D.Dict (Set Vertex)) (D.Dict (Set Vertex))

-- Provided for completeness, but for efficiency we avoid them.
instance Semigroup GraphImpl where
   append (GraphImpl out1 in1) (GraphImpl out2 in2) =
      GraphImpl (D.unionWith S.union out1 out2) (D.unionWith S.union in1 in2)

instance Monoid GraphImpl where
   mempty = GraphImpl D.empty D.empty

empty :: GraphImpl
empty = mempty

instance Graph GraphImpl where
   remove α (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = S.delete α <$> D.delete (unwrap α) out
      in' = S.delete α <$> D.delete (unwrap α) in_

   extend α αs (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = D.insert (unwrap α) αs out
      in' = foldl (\d α' -> D.insertWith S.union (unwrap α') (S.singleton α) d)
         (D.insert (unwrap α) S.empty in_)
         αs

   connectOut α β (GraphImpl out in_) = GraphImpl out' in'
      where
      out' = D.update (S.insert β >>> Just) (unwrap α)
         (D.insertWith S.union (unwrap β) S.empty out)
      in' = D.insertWith S.union (unwrap β) (S.singleton α) in_

   connectIn α β g = opp (connectOut β α (opp g))

   outN (GraphImpl out _) α = D.lookup (unwrap α) out # definitely "in graph"
   inN g = outN (opp g)

   elem (GraphImpl out _) α = isJust (D.lookup (unwrap α) out)
   size (GraphImpl out _) = D.size out

   opp (GraphImpl out in_) = GraphImpl in_ out

   discreteG αs = GraphImpl discreteM discreteM
      where
      discreteM = D.fromFoldable $ S.map (\α -> unwrap α × S.empty) αs

instance Show GraphImpl where
   show (GraphImpl out in_) = "GraphImpl (" <> show out <> " × " <> show in_ <> ")"
