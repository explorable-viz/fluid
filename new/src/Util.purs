module Util where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus, empty)
import Data.Bifunctor (bimap)
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

error :: String -> ∀ a . a
error msg = unsafePerformEffect $ throw msg

assert :: Boolean -> ∀ a . a -> a
assert true = identity
assert false = \_ -> error "Assertion failure"

absurd :: String
absurd = "absurd"

unimplemented :: String
unimplemented = "unimplemented"

whenever :: forall a . Boolean -> a -> Maybe a
whenever false = const Nothing
whenever true  = Just

fromJust :: forall a . String -> Maybe a -> a
fromJust _ (Just a) = a
fromJust msg Nothing  = error msg

onlyIf :: Boolean -> forall m a . MonadPlus m => a -> m a
onlyIf true    = pure
onlyIf false   = const empty

type MayFail a = String + a

report :: String -> forall a . MayFail a
report = Left

successful :: forall a . MayFail a -> a
successful (Left msg)   = error msg
successful (Right x)    = x

successfulWith :: String -> forall a . MayFail a -> a
successfulWith msg = successful <<< with msg

-- If the property fails, add an extra error message.
with :: String -> forall a . MayFail a -> MayFail a
with msg = bimap (\msg' -> msg' <> if msg == "" then "" else ("\n" <> msg)) identity

check :: Boolean -> String -> MayFail Unit
check true _      = pure unit
check false msg   = report msg

mayEq :: forall a . Eq a => a -> a -> Maybe a
mayEq x x' = whenever (x == x') x

mustEq :: forall a . Eq a => a -> a -> a
mustEq x x' = fromJust "Must be equal" $ x ≟ x'

unionWithMaybe :: forall a b . Ord a => (b -> b -> Maybe b) -> Map a b -> Map a b -> Map a (Maybe b)
unionWithMaybe f m m' = unionWith (\x -> lift2 f x >>> join) (map Just m) (map Just m')

mayFailEq :: forall a . Show a => Eq a => a -> a -> MayFail a
mayFailEq x x' = note (show x <> " ≠ " <> show x') $ x ≟ x'

infixl 5 mayEq as ≟
infixl 5 mayFailEq as ≞
infixl 5 mustEq as ≜

-- Could be more efficient
intersperse :: forall a . a -> List a -> List a
intersperse x xs = intercalate (pure x) $ map pure xs

om :: forall a b c m . Monad m => (a -> b -> m c) -> m a -> b -> m c
om f m x = m >>= flip f x

bind2Flipped :: forall m a b c . Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2Flipped f x y = join $ lift2 f x y

infixr 1 bind2Flipped as =<<<

-- Data types that have a List embedding.
class QuaList a where
   toList :: a -> List a
   fromList :: List a -> a

quaList :: forall a . QuaList a => (List a -> List a) -> a -> a
quaList f x = fromList $ f (toList x)

quaList2 :: forall a . QuaList a => (List a -> List a -> List a) -> a -> a -> a
quaList2 f x y = fromList $ f (toList x) (toList y)
