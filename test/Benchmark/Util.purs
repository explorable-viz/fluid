module Benchmark.Util where

import Prelude

import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (intersperse, fromFoldable) as A
import Data.List (fold)
import Data.Map (Map, unionWith, fromFoldable, keys, values)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Microtime (microtime)
import Util (type (×), (×))

data BenchRow = BenchRow TraceRow GraphRow

newtype BenchAcc = BenchAcc (Array (String × BenchRow))

type WithBenchAcc g a = WriterT BenchAcc g a

runWithBenchAcc :: forall g a. Monad g => WithBenchAcc g a -> g (a × BenchAcc)
runWithBenchAcc = runWriterT

derive newtype instance Semigroup BenchAcc
derive newtype instance Monoid BenchAcc

type TraceRow = Map String Number
-- { tEval :: Number
-- , tBwd :: Number
-- , tFwd :: Number
-- }

type GraphRow = Map String Number

-- { tEval :: Number
-- , tBwd :: Number
-- , tBwdDual :: Number
-- , tBwdAll :: Number
-- , tFwd :: Number
-- , tFwdDual :: Number
-- , tFwdAsDemorgan :: Number
-- }

instance Show BenchAcc where
   show (BenchAcc rows) =
      fold $ A.intersperse "\n" ([ showHeader ] <> (showRow <$> rows))
      where
      BenchRow tr_empty gr_empty = mempty

      showHeader :: String
      showHeader =
         fold $ A.intersperse "," ([ "Test-Name" ] <> A.fromFoldable (keys tr_empty) <> A.fromFoldable (keys gr_empty))

      showRow :: String × BenchRow -> String
      showRow (test_name × (BenchRow trRow grRow)) =
         fold $ A.intersperse "," ([ test_name ] <> (show <$> A.fromFoldable (values trRow <> values grRow)))

instance Semigroup BenchRow where
   append (BenchRow trRow1 gRow1) (BenchRow trRow2 gRow2) =
      BenchRow
         (unionWith (+) trRow1 trRow2)
         (unionWith (+) gRow1 gRow2)

instance Monoid BenchRow where
   mempty = BenchRow
      (fromFoldable [ ("Trace-Eval" × 0.0), ("Trace-Bwd" × 0.0), ("Trace-Fwd" × 0.0) ])
      (fromFoldable [ ("Graph-Eval" × 0.0), ("Graph-Bwd" × 0.0), ("Graph-Fwd" × 0.0), ("Graph-BwdDual" × 0.0), ("Graph-BwdAll" × 0.0), ("Graph-FwdDual" × 0.0), ("Graph-FwdAsDeMorgan" × 0.0) ])

tdiff :: Number -> Number -> Number
tdiff x y = sub y x

preciseTime :: forall m. MonadEffect m => m Number
preciseTime = liftEffect microtime

bench :: forall m a. MonadEffect m => (Unit -> m a) -> m (a × Number)
bench prog = do
   t1 <- preciseTime
   a <- prog unit
   t2 <- preciseTime
   pure (a × tdiff t1 t2)