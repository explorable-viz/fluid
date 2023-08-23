module Desugarable where

import Prelude
import Lattice (Raw, class JoinSemilattice, class BoundedJoinSemilattice)
import Util (MayFailT)

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall a m. Monad m => JoinSemilattice a => s a -> MayFailT m (e a)
   desugBwd :: forall a. BoundedJoinSemilattice a => e a -> Raw s -> s a
