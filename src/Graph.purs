module Graph where

import Prelude

import Control.Monad.State (class MonadState, State, StateT, get, put, runState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List (fromFoldable, filter, elem, concat) as L
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (first, second)
import Data.Set (Set)
import Data.Set (delete, empty, map, singleton, union) as S
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (fst)
import Foreign.Object (Object, delete, empty, fromFoldable, lookup, singleton, size, unionWith) as SM
import Util (Endo, MayFailT, (×), type (×))

type SMap = SM.Object
type Edge = Vertex × Vertex

-- Graphs form a semigroup but we don't actually rely on that (for efficiency).
class Monoid g <= Graph g where
   extend :: Vertex -> Set Vertex -> Endo g
   outN :: g -> Vertex -> Maybe (Set Vertex)
   inN :: g -> Vertex -> Maybe (Set Vertex)
   singleton :: Vertex -> Set Vertex -> g
   remove :: Vertex -> Endo g
   opp :: Endo g
   allocate :: g -> Vertex
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
outE' graph α = case outN graph α of
   Just set -> L.fromFoldable $ S.map (\node -> α × node) set
   Nothing -> Nil

outE :: forall g. Graph g => Set Vertex -> g -> List Edge
outE αs g =
   let
      allOut = L.concat (map (\α -> outE' g α) (L.fromFoldable αs))
   in
      L.filter (\(e1 × e2) -> (L.elem e1 αs || L.elem e2 αs)) allOut

inE' :: forall g. Graph g => g -> Vertex -> List Edge
inE' graph α = case inN graph α of
   Just set -> L.fromFoldable $ S.map (\node -> node × α) set
   Nothing -> Nil

inE :: forall g. Graph g => Set Vertex -> g -> List Edge
inE αs g =
   let
      allIn = L.concat (map (\α -> inE' g α) (L.fromFoldable αs))
   in
      L.filter (\(e1 × e2) -> L.elem e1 αs || L.elem e2 αs) allIn

elem :: forall g. Graph g => g -> Vertex -> Boolean
elem graph α =
   case outN graph α of
      Just _ -> true
      Nothing -> false

bwdSlice :: forall g. Graph g => Set Vertex -> g -> g
bwdSlice αs parent = bwdSlice' parent startG edges
   where
   startG = discreteG αs
   edges = outE αs parent

bwdSlice' :: forall g. Graph g => g -> g -> List Edge -> g
bwdSlice' parent g ((s × t) : es) =
   if elem g t then
      let
         newG = extend s (S.singleton t) g
      in
         bwdSlice' parent newG es
   else
      let
         newG = extend s (S.singleton t) g
         newEs = append es (L.fromFoldable (outE' parent t))
      in
         bwdSlice' parent newG newEs
   where
   append :: forall a. List a -> List a -> List a
   append Nil xs = xs
   append (y : ys) xs = append ys (y : xs)

bwdSlice' _ g Nil = g

fwdSlice :: forall g. Graph g => Set Vertex -> g -> g
fwdSlice αs parent = fst $ fwdEdges parent startG mempty edges
   where
   startG = discreteG αs
   edges = inE αs parent

fwdEdges :: forall g. Graph g => g -> g -> g -> List Edge -> g × g
fwdEdges parent currSlice pending ((s × t) : es) =
   let
      (g' × h') = fwdVertex parent currSlice (extend s (S.singleton t) pending) s
   in
      fwdEdges parent g' h' es
fwdEdges _ currSlice pending Nil = currSlice × pending

fwdVertex :: forall g. Graph g => g -> g -> g -> Vertex -> g × g
fwdVertex parent currSlice pending α =
   let
      currNeighbors = outN pending α
   in
      if currNeighbors == (outN parent α) then
         case currNeighbors of
            Just αs -> fwdEdges parent (extend α αs currSlice) (remove α pending) (inE' parent α)
            Nothing -> fwdEdges parent (extend α S.empty currSlice) pending (inE' parent α)
      else currSlice × pending

derive instance Eq Vertex
derive instance Ord Vertex
derive instance Newtype Vertex _

instance Show Vertex where
   show (Vertex α) = "Vertex " <> α

-- GraphImpl Specifics
data GraphImpl = GraphImpl (SMap (Set Vertex)) (SMap (Set Vertex))

instance Semigroup GraphImpl where
   append (GraphImpl out1 in1) (GraphImpl out2 in2) =
      GraphImpl (SM.unionWith S.union out1 out2) (SM.unionWith S.union in1 in2)

instance Monoid GraphImpl where
   mempty = GraphImpl SM.empty SM.empty

empty :: GraphImpl
empty = mempty

instance Graph GraphImpl where
   allocate (GraphImpl out _) = Vertex α
      where
      α = show $ 1 + (SM.size out)
   remove (Vertex α) (GraphImpl out in_) =
      let
         newOutN = map (S.delete (Vertex α)) (SM.delete α out)
         newInN = map (S.delete (Vertex α)) (SM.delete α in_)
      in
         GraphImpl newOutN newInN
   extend α αs (GraphImpl out in_) = GraphImpl newOut newIn
      where
      newOut = SM.unionWith S.union out (starInOut α αs)
      newIn = SM.unionWith S.union in_ (starInIn α αs)

   outN (GraphImpl out _) (Vertex α) = SM.lookup α out
   inN (GraphImpl _ in_) (Vertex α) = SM.lookup α in_

   singleton α αs = GraphImpl (starInOut α αs) (starInIn α αs)
   opp (GraphImpl out in_) = GraphImpl in_ out
   discreteG αs =
      let
         pairs = S.map (\(Vertex α) -> α × S.empty) αs
         discreteM = SM.fromFoldable pairs
      in
         GraphImpl discreteM discreteM

-- prototype attempts at more efficiently implementing the above operations
starInOut :: Vertex -> Set Vertex -> SMap (Set Vertex)
starInOut (Vertex α) αs = SM.unionWith S.union (SM.singleton α αs) (star αs)
   where
   star :: Set Vertex -> SMap (Set Vertex)
   star αs' = SM.fromFoldable $ S.map (\(Vertex α') -> α' × S.empty) αs'

starInIn :: Vertex -> Set Vertex -> SMap (Set Vertex)
starInIn v@(Vertex α) αs = SM.unionWith S.union (SM.singleton α S.empty) (star v αs)
   where
   star :: Vertex -> Set Vertex -> SMap (Set Vertex)
   star α' αs' = SM.fromFoldable $ S.map (\(Vertex α'') -> α'' × (S.singleton α')) αs'

inStar :: Vertex -> Set Vertex -> GraphImpl
inStar α αs = opp (outStar α αs)

outStar :: Vertex -> Set Vertex -> GraphImpl
outStar α αs = GraphImpl (starInOut α αs) (starInIn α αs)

instance Show GraphImpl where
   show (GraphImpl out in_) = "GraphImpl (" <> show out <> " × " <> show in_ <> ")"
