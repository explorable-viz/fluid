module Desugarable where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Lattice (Raw, class JoinSemilattice, class BoundedJoinSemilattice)
import Util (MayFail, type (×))

wrapSugar :: forall s e. Desugarable s e => Raw s -> Sugar' e
wrapSugar s = Sugar' (\k -> k s)

unwrapSugar :: forall s e. Desugarable s e => Sugar' e -> Raw s
unwrapSugar (Sugar' k) = k unsafeCoerce

newtype Sugar' e = Sugar' (forall r. (forall s. Desugarable s e => Raw s -> r) -> r)

class FromSugar e where
   fromSug :: forall a. Sugar' e -> e a -> e a
   toSug :: forall a. e a -> Sugar' e × e a

class (Functor s, Functor e) <= Desugarable s e | s -> e where
   desug :: forall a. JoinSemilattice a => s a -> MayFail (e a)
   desugBwd :: forall a. BoundedJoinSemilattice a => e a -> Raw s -> s a
