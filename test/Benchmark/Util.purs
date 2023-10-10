module Benchmark.Util where

import Prelude

import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array (intersperse, fromFoldable) as A
import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Int (toNumber)
import Data.List (fold)
import Data.Map (Map, singleton, unionWith, keys, values)
import Data.Map (empty) as M
import Data.Newtype (class Newtype, over2)
import Data.Tuple (snd)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Microtime (microtime)
import Util (type (×), (×))

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

tdiff :: Number -> Number -> Number
tdiff x y = sub y x

preciseTime :: forall m. MonadEffect m => m Number
preciseTime = liftEffect microtime

bench :: forall m a. MonadEffect m => MonadWriter BenchRow m => String -> (Unit -> m a) -> m a
bench name prog = do
   t1 <- preciseTime
   r <- prog unit
   t2 <- preciseTime
   tell (BenchRow $ singleton name (tdiff t1 t2))
   pure r

divRow :: BenchRow -> Int -> BenchRow
divRow (BenchRow row) n = BenchRow (map (_ `div` toNumber n) row)

