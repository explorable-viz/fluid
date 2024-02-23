module Test.Benchmark.Util where

import Prelude

import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array (intersperse, fromFoldable) as A
import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.List (List(..), fold, length, union)
import Data.List (singleton) as L
import Data.Map (Map, singleton, unionWith, keys, values)
import Data.Map (empty) as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over2)
import Data.Number (pow, sqrt)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Graph (class Graph, size)
import Pretty (class Pretty, prettyP)
import Util (type (×), EffectError, Thunk, debug, force, (×))

logAs :: forall m. MonadEffect m => String -> String -> m Unit
logAs tag s = log $ tag <> ": " <> s

newtype BenchAcc = BenchAcc (NonEmptyArray (String × BenchRow))

instance Show BenchAcc where
   show (BenchAcc rows) =
      fold $ A.intersperse "\n" ([ showHeader ] <> (toArray $ showRow <$> rows))
      where
      BenchRow firstRow = head rows # snd

      showHeader :: String
      showHeader =
         fold $ A.intersperse "," ([ "Test-Name" ] <> A.fromFoldable (keys firstRow))

      showRow :: String × BenchRow -> String
      showRow (test_name × (BenchRow row)) =
         fold $ A.intersperse "," ([ test_name ] <> (show <$> A.fromFoldable (values row)))

newtype BenchRow = BenchRow (Map String (List Number))

derive instance Newtype BenchRow _

instance Semigroup BenchRow where
   append = unionWith union `flip over2` BenchRow

instance Monoid BenchRow where
   mempty = BenchRow M.empty

foreign import microtime :: Effect Number

microtime' :: forall m. MonadEffect m => m Number
microtime' = liftEffect microtime

time :: forall m a. MonadEffect m => Thunk (m a) -> m (Number × a)
time m = do
   t1 <- microtime'
   x <- force m
   t2 <- microtime'
   pure (t2 `sub` t1 × x)

logTimeWhen :: forall m a. MonadEffect m => Boolean -> String -> Thunk (m a) -> m a
logTimeWhen false _ m = force m
logTimeWhen true msg m = do
   t × x <- time m
   logAs msg (show t)
   pure x

benchmarkLog :: forall m a. MonadWriter BenchRow m => Pretty a => String -> Thunk (m a) -> EffectError m a
benchmarkLog name = benchmark' name (Just prettyP)

benchmark :: forall m a. MonadWriter BenchRow m => String -> Thunk (m a) -> EffectError m a
benchmark name = benchmark' name Nothing

benchmark' :: forall m a. MonadWriter BenchRow m => String -> Maybe (a -> String) -> Thunk (m a) -> EffectError m a
benchmark' name show_opt m = do
   when debug.logging $ log ("**** " <> name)
   t × x <- time m
   when debug.logging $
      case show_opt of
         Nothing -> pure unit
         Just show -> logAs name (show x)
   tell (BenchRow $ singleton name (L.singleton t))
   pure x

recordGraphSize :: forall g m. Graph g => MonadWriter BenchRow m => g -> m Unit
recordGraphSize g =
   tell (BenchRow $ singleton "Graph-Nodes" (L.singleton $ toNumber $ size g))

-- The changes here are definitely some kind of tech debt
divRow :: BenchRow -> Int -> BenchRow
divRow (BenchRow row) n = BenchRow ((\x -> (Cons ((sum x) `div` toNumber n) (L.singleton (stdErr x)))) <$> row)

stdDev :: List Number -> Number
stdDev nums = sqrt $ mean deviation
   where
   average = mean nums
   deviation = map (\x -> pow (x - average) 2.0) nums

stdErr :: List Number -> Number
stdErr nums = errval
   where
   root_n = sqrt (toNumber $ length nums)
   errval = stdDev nums / root_n

mean :: List Number -> Number
mean nums = sum nums `div` (toNumber $ length nums)