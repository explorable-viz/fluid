module TypeNat where

foreign import kind Nat
foreign import kind Boolean

foreign import data Zero :: Nat
foreign import data Succ :: Nat → Nat

class Iterate (n :: Nat) (f :: Type -> Type) (a :: Type) (t :: Type) | n f a -> t

instance iterateZero :: Iterate Zero f a (f a)
instance iterateSucc :: Iterate n f a t => Iterate (Succ n) f a (f t)
