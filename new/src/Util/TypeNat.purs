module TypeNat where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Exists :: (Nat -> Type -> Type) -> Type

mkExists :: forall f n a . f n a -> Exists f
mkExists = unsafeCoerce

foreign import kind Nat
foreign import kind Boolean

foreign import data Zero :: Nat
foreign import data Succ :: Nat → Nat

data NProxy (a :: Nat) = NProxy

class Iterate (n :: Nat) (f :: Type -> Type) (a :: Type) (t :: Type) | n f a -> t

instance iterateZero :: Iterate Zero f a a
instance iterateSucc :: Iterate n f a t => Iterate (Succ n) f a (f t)

data Elims' (n :: Nat) t =
   Elims' t

blah2 :: Elims' (Succ Zero) String
blah2 = Elims' "hello"

type Elims = Exists Elims'

data Elim k =
   Constr Elims
