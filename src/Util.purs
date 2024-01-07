module Util where

import Prelude hiding (absurd)

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (Except, ExceptT, runExcept)
import Control.MonadPlus (class Alternative, guard)
import Data.Array ((!!), updateAt)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.List (List(..), (:), intercalate)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map)
import Data.Map (lookup, unionWith) as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong (class Strong, (&&&), (***))
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple (Tuple(..), fst, snd)
import Debug (trace)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, message)
import Effect.Exception (error) as E
import Effect.Unsafe (unsafePerformEffect)

debug
   :: { logging :: Boolean -- logging via "log"; requires an effect context
      , tracing :: Boolean -- tracing via "trace"; no effect context required
      }

debug =
   { logging: true
   , tracing: true
   }

type Thunk a = Unit -> a -- similar to Lazy but without datatype

force :: forall a. Thunk a -> a
force = (_ $ unit)

type 𝔹 = Boolean

-- Surely × should have higher precedence than + by convention..
infixr 6 type Tuple as × -- standard library has \/
infixr 6 Tuple as ×

-- Prefer this pattern to the variant in Data.Tuple.Nested.
tuple3 :: forall a b c. a -> b -> c -> a × b × c
tuple3 a b c = a × b × c

uncurry3 :: forall a b c r. (a -> b -> c -> r) -> a × b × c -> r
uncurry3 f (a × b × c) = f a b c

infixr 6 type Either as + -- standard library has \/

type AffError m a = MonadAff m => MonadError Error m => m a
type EffectError m a = MonadEffect m => MonadError Error m => m a

-- Rethink: has same name as Effect.Exception.error but without the type :-o
error :: ∀ a. String -> a
error msg = unsafePerformEffect (throw msg)

shapeMismatch :: forall a. Thunk a
shapeMismatch _ = error "Shape mismatch"

throw :: forall m a. MonadThrow Error m => String -> m a
throw = throwError <<< E.error

assert :: ∀ a. Boolean -> Endo a
assert = assertWith ""

assertWith :: ∀ a. String -> Boolean -> Endo a
assertWith _ true = identity
assertWith msg false = \_ -> error ("Assertion failure: " <> msg)

assertWhen :: ∀ a. Boolean -> String -> Thunk Boolean -> Endo a
assertWhen false _ = const identity
assertWhen true msg = force >>> assertWith msg

validate :: ∀ a. String -> (a -> Boolean) -> Endo a
validate = validateWhen true

validateWhen :: ∀ a. Boolean -> String -> (a -> Boolean) -> Endo a
validateWhen b msg p a = assertWhen b msg (\_ -> p a) a

-- Debug.spyWith doesn't seem to work
spyWhen :: forall a. Boolean -> String -> Endo a
spyWhen b msg = spyWhenWith b msg identity

spyWhenWith :: forall a b. Boolean -> String -> (a -> b) -> Endo a
spyWhenWith true msg show x | debug.tracing == true =
   trace (msg <> ":") \_ -> trace (show x) (const x)
spyWhenWith _ _ _ x = x

spyFunWhenWith :: forall a b c d1 d2. Boolean -> String -> (a × b -> d1) -> (c -> d2) -> Endo (a × b -> c)
spyFunWhenWith b s showIn showOut f (x × y) =
   f ((x × y) # spyWhenWith b (s <> " input") showIn) # spyWhenWith b (s <> " output") showOut

-- Prefer this to Debug.spy (similar to spyWith).
spy :: forall a. String -> Endo a
spy = spyWhen true

spyWith :: forall a b. String -> (a -> b) -> Endo a
spyWith = spyWhenWith true

traceWhen :: forall m. Applicative m => Boolean -> String -> m Unit
traceWhen true msg | debug.tracing == true = trace msg \_ -> pure unit
traceWhen _ _ = pure unit

absurd :: String
absurd = "absurd"

unimplemented :: String
unimplemented = "unimplemented"

whenever :: forall a. Boolean -> a -> Maybe a
whenever false = const Nothing
whenever true = Just

definitely :: forall a. String -> Maybe a -> a
definitely _ (Just a) = a
definitely msg Nothing = error ("definitely " <> msg)

definitely' :: forall a. Maybe a -> a
definitely' = definitely absurd

get :: forall k v. Ord k => k -> Map k v -> v
get k = definitely' <<< M.lookup k

onlyIf :: forall m a. Bind m => Alternative m => Boolean -> a -> m a
onlyIf b a = do
   guard b
   pure a

type MayFail a = Except Error a
type MayFailT m = ExceptT Error m

orElse :: forall a m. MonadThrow Error m => String -> Maybe a -> m a
orElse s Nothing = throw s
orElse _ (Just x) = pure x

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft = flip bimap identity

successful :: forall a. MayFail a -> a
successful = runExcept >>> case _ of
   Right x -> x
   Left e -> error $ show e

successfulWith :: String -> forall a. MayFail a -> a
successfulWith msg = successful <<< with msg

-- If the property fails, add an extra error message.
with :: forall a m. MonadError Error m => String -> Endo (m a)
with msg m = catchError m \e ->
   let msg' = message e in throw $ msg' <> if msg == "" then "" else ("\n" <> msg)

check :: forall m. MonadThrow Error m => Boolean -> String -> m Unit
check true = const $ pure unit
check false = throw

-- Like shouldSatisfy in Test.Spec.Assertions but with error message.
checkSatisfies :: forall m a. MonadThrow Error m => Show a => String -> a -> (a -> Boolean) -> m Unit
checkSatisfies msg x pred =
   unless (pred x) $
      throw (show x <> " doesn't satisfy " <> msg)

mayEq :: forall a. Eq a => a -> a -> Maybe a
mayEq x x' = whenever (x == x') x

mustEq :: forall a. Eq a => Show a => a -> Endo a
mustEq x x' = definitely (show x <> " equal to " <> show x') (x ≟ x')

mustGeq :: forall a. Ord a => Show a => a -> Endo a
mustGeq x x' = definitely (show x <> " greater than " <> show x') (whenever (x >= x') x)

unionWithMaybe :: forall a b. Ord a => (b -> b -> Maybe b) -> Map a b -> Map a b -> Map a (Maybe b)
unionWithMaybe f m m' = M.unionWith (\x -> lift2 f x >>> join) (Just <$> m) (Just <$> m')

mayFailEq :: forall a m. MonadThrow Error m => Show a => Eq a => a -> a -> m a
mayFailEq x x' = x ≟ x' # orElse (show x <> " ≠ " <> show x')

infixl 4 mayEq as ≟
infixl 4 mayFailEq as ≞
infixl 4 mustEq as ≜
infixl 4 mustGeq as ⪄

-- could be more efficient
intersperse :: forall a. a -> Endo (List a)
intersperse x xs = intercalate (pure x) (pure <$> xs)

om :: forall a b c m. Monad m => (a -> b -> m c) -> m a -> b -> m c
om f m x = m >>= flip f x

bind2Flipped :: forall m a b c. Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2Flipped f x y = join (lift2 f x y)

infixr 1 bind2Flipped as =<<<

type Endo a = a -> a

-- version of this in Data.Array uses unsafePartial
unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex xs i = definitely "index within bounds" (xs !! i)

unsafeUpdateAt :: forall a. Int -> a -> Endo (Array a)
unsafeUpdateAt i x = updateAt i x >>> definitely "index within bounds"

infixl 8 unsafeIndex as !

nonEmpty :: forall a. List a -> NonEmptyList a
nonEmpty Nil = error absurd
nonEmpty (x : xs) = NonEmptyList (x :| xs)

-- Also defined in Data.Profunctor.Monoidal, but perhaps not "standard library"
dup :: forall a. a -> a × a
dup x = x × x

unzip :: forall t a b. Functor t => t (a × b) -> t a × t b
unzip = map fst &&& map snd

both :: forall a b c. Category a => Strong a => a b c -> a (b × b) (c × c)
both f = f *** f

assoc1 :: forall a b c. (a × b) × c -> a × (b × c)
assoc1 ((a × b) × c) = a × (b × c)

assoc2 :: forall a b c. a × (b × c) -> (a × b) × c
assoc2 (a × (b × c)) = (a × b) × c

-- Not sure what provision there is for composition of functors with types
data WithTypeLeft (t :: Type) (f :: Type -> Type) a = WithTypeLeft t (f a)

infixr 6 type WithTypeLeft as <×|
infixr 6 WithTypeLeft as <×|

derive instance Functor f => Functor (t <×| f)

-- Haven't found this yet in PureScript
concatM :: forall f m a. Foldable f => Monad m => f (a -> m a) -> a -> m a
concatM = foldr (>=>) pure

infixr 7 Set.intersection as ∩
infixr 6 Set.union as ∪
infix 5 Set.difference as \\
infix 5 Set.member as ∈
infixl 4 Set.subset as ⊆

-- Simplify overloading.
class Singleton f where
   singleton :: forall a. a -> f a

instance Singleton Array where
   singleton = pure

instance Singleton List where
   singleton = pure

instance Singleton NonEmptyList where
   singleton = pure

instance Singleton Set where
   singleton = Set.singleton

instance Singleton NonEmptySet where
   singleton = NonEmptySet.singleton
