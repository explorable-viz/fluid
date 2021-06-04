module Bindings2 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Bindings (Bindings(..), (:+:))
import Bindings ((↦)) as B
import Lattice (
   class BoundedSlices, class Expandable, class JoinSemilattice, class Slices, botOf, definedJoin, expand, maybeJoin, neg
)
import Util (type (×), (≜), (≞))
import Util.SnocList (SnocList(..), (:-))

type Var = String -- newtype?

varAnon = "_" :: Var

newtype Bind a = Bind (Var × a)
type Bindings2 a = SnocList (Bind a)

derive instance newtypeBind :: Newtype (Bind a) _
derive instance functorBind :: Functor Bind

infix 6 Tuple as ↦

instance expandableBind :: Expandable a => Expandable (Bind a) where
   expand (Bind (x ↦ v)) (Bind (x' ↦ v')) = Bind ((x ≜ x') ↦ expand v v')

instance joinSemilatticeBind :: Slices a => JoinSemilattice (Bind a) where
   join = definedJoin
   neg = (<$>) neg

instance slicesBind :: Slices a => Slices (Bind a) where
   maybeJoin (Bind (x ↦ v)) (Bind (y ↦ v')) = Bind <$> ((↦) <$> (x ≞ y) <*> maybeJoin v v')

instance boundedSlicesBind :: BoundedSlices a => BoundedSlices (Bind a) where
   botOf = (<$>) botOf

-- Temporary conversion from new bindings to old.
asBindings :: forall t a . Bindings2 (t a) -> Bindings t a
asBindings Lin = Empty
asBindings (ρ :- Bind (x ↦ v)) = asBindings ρ :+: x B.↦ v

asBindings2 :: forall t a . Bindings t a -> Bindings2 (t a)
asBindings2 Empty = Lin
asBindings2 (ρ :+: x B.↦ v) = asBindings2 ρ :- Bind (x ↦ v)
