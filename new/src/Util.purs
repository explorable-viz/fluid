module Util where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.List (List, intercalate)
import Data.Map (Map, unionWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

infixr 6 type Tuple as ×
infixr 6 Tuple as ×

data T3 a b c = T3 a b c

error :: ∀ a . String -> a
error = unsafePerformEffect <<< throw

assert :: ∀ a . Boolean -> a -> a
assert true = identity
assert false = \_ -> error "Assertion failure"

absurd :: String
absurd = "absurd"

fromBool :: forall a . Boolean -> a -> Maybe a
fromBool false = const Nothing
fromBool true  = Just

toBool :: forall a . Maybe a -> Boolean
toBool (Just x) = true
toBool Nothing  = false

fromJust :: forall a . String -> Maybe a -> a
fromJust _ (Just a) = a
fromJust msg Nothing  = error msg

type MayFail a = Either String a

successful :: forall a . MayFail a -> a
successful (Left msg) = error msg
successful (Right b)  = b

mayEq :: forall a . Eq a => a -> a -> Maybe a
mayEq x x' = if x == x' then Just x else Nothing

mustEq :: forall a . Eq a => a -> a -> a
mustEq x x' = fromJust "Must be equal" $ x ≟ x'

unionWithMaybe :: forall a b . Ord a => (b -> b -> Maybe b) -> Map a b -> Map a b -> Map a (Maybe b)
unionWithMaybe f m m' = unionWith (\x -> lift2 f x >>> join) (map Just m) (map Just m')

eitherEq :: forall a . Show a => Eq a => a -> a -> MayFail a
eitherEq x x' = if x == x' then pure x else Left $ show x <> " ≠ " <> show x'

infixl 5 mayEq as ≟
infixl 5 mustEq as ≜

-- Could be more efficient
intersperse :: forall a . a -> List a -> List a
intersperse x xs = intercalate (pure x) $ map pure xs

om :: forall a b c m . Monad m => (a -> b -> m c) -> m a -> b -> m c
om f m x = m >>= flip f x
