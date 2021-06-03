module Bindings2 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Bindings (Bindings(..), (:+:))
import Bindings ((↦)) as B
import Lattice (class Expandable, class JoinSemilattice, class Slices, definedJoin, expand, maybeJoin, neg)
import Util (type (×), (≜), (≞))
import Util.SnocList (SnocList(..), (:-))

type Var = String -- newtype?

newtype Bind a = Bind (Var × a)
type Bindings2 a = SnocList (Bind a)

derive instance newtypeBind :: Newtype (Bind a) _
derive instance functorBind :: Functor Bind

infix 6 Tuple as ↦

instance expandableBind :: Expandable a => Expandable (Bind a) where
   expand (Bind (x ↦ v)) (Bind (x' ↦ v')) = Bind ((x ≜ x') ↦ expand v v')

instance joinSemilatticeBindings :: Slices a => JoinSemilattice (Bind a) where
   join = definedJoin
   neg = (<$>) neg

instance slicesBind :: Slices a => Slices (Bind a) where
   maybeJoin (Bind (x ↦ v)) (Bind (y ↦ v')) = Bind <$> ((↦) <$> (x ≞ y) <*> maybeJoin v v')

-- Temporary conversion from new bindings to old.
asBindings :: forall t a . Bindings2 (t a) -> Bindings t a
asBindings SnocNil = Empty
asBindings (ρ :- Bind (x ↦ v)) = asBindings ρ :+: x B.↦ v

asBindings2 :: forall t a . Bindings t a -> Bindings2 (t a)
asBindings2 Empty = SnocNil
asBindings2 (ρ :+: x B.↦ v) = asBindings2 ρ :- Bind (x ↦ v)
