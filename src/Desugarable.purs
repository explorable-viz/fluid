module Desugarable where

import Prelude
import Lattice (Raw, class JoinSemilattice, class BoundedJoinSemilattice)
import Util (MayFail)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall a. JoinSemilattice a => s a -> MayFail (e a)
   desugBwd :: forall a. BoundedJoinSemilattice a => e a -> Raw s -> s a
