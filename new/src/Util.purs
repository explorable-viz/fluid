module Util where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus, empty)
import Data.Either (Either(..), note)
import Data.List (List, intercalate)
import Data.Map (Map, unionWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

infixr 7 type Tuple as ×
infixr 7 Tuple as ×

infixr 6 type Either as +

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

fromJust :: forall a . String -> Maybe a -> a
fromJust _ (Just a) = a
fromJust msg Nothing  = error msg

pureMaybe :: forall m . MonadPlus m => Maybe ~> m
pureMaybe Nothing    = empty
pureMaybe (Just x)   = pure x

pureIf :: forall m a . MonadPlus m => Boolean -> a -> m a
pureIf b = fromBool b >>> pureMaybe

type MayFail a = String + a

successful :: forall a . MayFail a -> a
successful (Left msg) = error msg
successful (Right b)  = b

mayEq :: forall a . Eq a => a -> a -> Maybe a
mayEq x x' = fromBool (x == x') x

mustEq :: forall a . Eq a => a -> a -> a
mustEq x x' = fromJust "Must be equal" $ x ≟ x'

unionWithMaybe :: forall a b . Ord a => (b -> b -> Maybe b) -> Map a b -> Map a b -> Map a (Maybe b)
unionWithMaybe f m m' = unionWith (\x -> lift2 f x >>> join) (map Just m) (map Just m')

mayFailEq :: forall a . Show a => Eq a => a -> a -> MayFail a
mayFailEq x x' = note (show x <> " ≠ " <> show x') $ x ≟ x'

infixl 5 mayEq as ≟
infixl 5 mustEq as ≜

-- Could be more efficient
intersperse :: forall a . a -> List a -> List a
intersperse x xs = intercalate (pure x) $ map pure xs

om :: forall a b c m . Monad m => (a -> b -> m c) -> m a -> b -> m c
om f m x = m >>= flip f x
