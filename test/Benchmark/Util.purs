module Benchmark.Util where

import Prelude

import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array (intersperse, fromFoldable) as A
import Data.List (fold)
import Data.Map (Map, singleton, unionWith, fromFoldable, keys, values)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Microtime (microtime)
import Util (type (×), (×))

newtype BenchRow = BenchRow (Map String Number)

newtype BenchAcc = BenchAcc (Array (String × BenchRow))

instance Show BenchAcc where
   show (BenchAcc rows) =
      fold $ A.intersperse "\n" ([ showHeader ] <> (showRow <$> rows))
      where
      BenchRow empty_row = mempty

      showHeader :: String
      showHeader =
         fold $ A.intersperse "," ([ "Test-Name" ] <> A.fromFoldable (keys empty_row))

      showRow :: String × BenchRow -> String
      showRow (test_name × (BenchRow row)) =
         fold $ A.intersperse "," ([ test_name ] <> (show <$> A.fromFoldable (values row)))

instance Semigroup BenchRow where
   append (BenchRow row1) (BenchRow row2) = BenchRow (unionWith (+) row1 row2)

instance Monoid BenchRow where
   mempty = BenchRow
      (fromFoldable [ ("Trace-Eval" × 0.0), ("Trace-Bwd" × 0.0), ("Trace-Fwd" × 0.0), ("Graph-Eval" × 0.0), ("Graph-Bwd" × 0.0), ("Graph-Fwd" × 0.0), ("Graph-BwdDual" × 0.0), ("Graph-BwdAll" × 0.0), ("Graph-FwdDual" × 0.0), ("Graph-FwdAsDeMorgan" × 0.0) ])

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
