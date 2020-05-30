module Util where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

data T3 a b c = T3 a b c

error :: ∀ a . String -> a
error = unsafePerformEffect <<< throw

todo :: String
todo = "todo"

absurd :: String
absurd = "absurd"

fromBool :: forall a . Boolean -> a -> Maybe a
fromBool false = const Nothing
fromBool true = Just
