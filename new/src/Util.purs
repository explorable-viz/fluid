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
fromBool true  = Just

toBool :: forall a . Maybe a -> Boolean
toBool (Just x) = true
toBool Nothing  = false

fromJust :: forall a . Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error absurd

type MayFail a = Either String a

successful :: forall a . MayFail a -> a
successful (Left msg) = error msg
successful (Right b)  = b

mayEq :: forall a . Eq a => a -> a -> Maybe a
mayEq x x' = if x == x' then Just x else Nothing

mustEq :: forall a . Eq a => a -> a -> a
mustEq x x' = if x == x' then x else error absurd

infixl 5 mayEq as ≟

infixl 5 mustEq as ≜