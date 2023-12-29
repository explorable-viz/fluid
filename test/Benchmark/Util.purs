module Test.Benchmark.Util where

import Prelude

import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array (intersperse, fromFoldable) as A
import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Int (toNumber)
import Data.List (fold)
import Data.Map (Map, singleton, unionWith, keys, values)
import Data.Map (empty) as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over2)
import Data.Tuple (snd)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Graph (class Graph, size)
import Pretty (class Pretty, prettyP)
import Test.Util.Microtime (microtime)
import Util (type (×), EffectError, (×), debug)

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

newtype BenchRow = BenchRow (Map String Number)

derive instance Newtype BenchRow _

instance Semigroup BenchRow where
   append = unionWith (+) `flip over2` BenchRow

instance Monoid BenchRow where
   mempty = BenchRow M.empty

benchmarkLog :: forall m a. MonadWriter BenchRow m => Pretty a => String -> (Unit -> m a) -> EffectError m a
benchmarkLog name = benchmark' name (Just prettyP)

benchmark :: forall m a. MonadWriter BenchRow m => String -> (Unit -> m a) -> EffectError m a
benchmark name = benchmark' name Nothing

benchmark' :: forall m a. MonadWriter BenchRow m => String -> Maybe (a -> String) -> (Unit -> m a) -> EffectError m a
benchmark' name show_opt m = do
   when debug.logging $ log ("**** " <> name)
   t1 <- preciseTime
   x <- m unit
   t2 <- preciseTime
   when debug.logging $
      case show_opt of
         Nothing -> pure unit
         Just show -> logAs name (show x)
   tell (BenchRow $ singleton name (t2 `sub` t1))
   pure x
   where
   preciseTime :: m Number
   preciseTime = liftEffect microtime

recordGraphSize :: forall g m. Graph g => MonadWriter BenchRow m => g -> m Unit
recordGraphSize g = do
   tell (BenchRow $ singleton "Graph-Nodes" (toNumber $ size g))

divRow :: BenchRow -> Int -> BenchRow
divRow (BenchRow row) n = BenchRow ((_ `div` toNumber n) <$> row)
