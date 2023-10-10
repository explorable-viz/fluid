module Ann where

import Prelude

erase :: forall t a. Functor t => t a -> Raw t
erase = (<$>) (const unit)

type 𝔹 = Boolean
type Raw (c :: Type -> Type) = c Unit
