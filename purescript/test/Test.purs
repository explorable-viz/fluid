module Test.Test where

import Prelude
import Effect (Effect)
import Effect.Exception (throw)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
   _ <- pure $ error "Doesn't fail"
   error' "Fails"

error :: ∀ a . String -> a
error = unsafeCoerce <<< throw

error' :: ∀ a . String -> Effect a
error' = throw
