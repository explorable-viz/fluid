module Desugarable where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Lattice (Raw, class JoinSemilattice, erase, class BoundedJoinSemilattice)
import Util (MayFail, type (×), (×))

wrapSugar :: forall s e. Desugarable s e => Raw s -> Sugar' e
wrapSugar sa = Sugar' (\ds -> ds sa)

unwrapSugar :: forall s e. Desugarable s e => Sugar' e -> Raw s
unwrapSugar (Sugar' k) = k unsafeCoerce

newtype Sugar' e = Sugar' (forall r. (forall s. Desugarable s e => Raw s -> r) -> r)

class FromSugar e where
   fromSug :: forall a. Sugar' e -> e a -> e a
   toSug :: forall a. e a -> Sugar' e × e a

class (Functor s, Functor e, FromSugar e) <= Desugarable (s :: Type -> Type) (e :: Type -> Type) | s -> e where
   desugFwd :: forall a. JoinSemilattice a => s a -> MayFail (e a)
   desugBwd :: forall a. BoundedJoinSemilattice a => e a -> Raw s -> s a

desugFwd' :: forall s e a. JoinSemilattice a => Desugarable s e => s a -> MayFail (e a)
desugFwd' x = fromSug (wrapSugar $ erase x) <$> desugFwd x

desugBwd' :: forall s e a. BoundedJoinSemilattice a => Desugarable s e => e a -> s a
desugBwd' exp = let (s × exp') = toSug exp in desugBwd exp' (unwrapSugar s)
