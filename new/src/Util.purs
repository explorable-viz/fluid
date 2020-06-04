module Util where

import Prelude
import Data.Either (Either(..))
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

type MayFail a = Either String a

successful :: forall a . MayFail a -> a
successful (Left msg) = error msg
successful (Right b) = b
