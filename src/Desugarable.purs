module Desugarable where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Lattice (Raw, class JoinSemilattice, erase, class BoundedJoinSemilattice)
import Util (MayFail, type (×), (×))

wrapSugar :: forall s e a. Desugarable s e => s a -> Sugar' e a
wrapSugar sa = Sugar' (\ds -> ds sa)

unwrapSugar :: forall s e a. Desugarable s e => Sugar' e a -> s a
unwrapSugar (Sugar' k) = k unsafeCoerce

runSugar :: forall e a. JoinSemilattice a => Sugar' e a -> MayFail (e a)
runSugar (Sugar' k) = k desugFwd

newtype Sugar' e (a :: Type) = Sugar' (forall r. (forall s. Desugarable s e => s a -> r) -> r)

class FromSugar e where
   fromSug :: forall a. Raw (Sugar' e) -> e a -> e a
   toSug :: forall a. e a -> Raw (Sugar' e) × e a

class (Functor s, Functor e, FromSugar e) <= Desugarable (s :: Type -> Type) (e :: Type -> Type) | s -> e where
   desugFwd :: forall a. JoinSemilattice a => s a -> MayFail (e a)
   desugBwd :: forall a. BoundedJoinSemilattice a => e a -> Raw s -> s a

instance Functor e => Functor (Sugar' e) where
   map :: forall a b. (a -> b) -> Sugar' e a -> Sugar' e b
   map f (Sugar' k) = Sugar' (\sug -> k (\sa -> sug (map f sa)))

desugFwd' :: forall s e a. JoinSemilattice a => Desugarable s e => s a -> MayFail (e a)
desugFwd' x = fromSug (erase (wrapSugar x)) <$> desugFwd x

desugBwd' :: forall s e a. BoundedJoinSemilattice a => Desugarable s e => e a -> s a
desugBwd' exp = let (s × exp') = toSug exp in desugBwd exp' (unwrapSugar s)