module Benchmark.Util where

import Prelude

import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (intersperse)
import Data.Foldable (fold)
import Data.JSDate (JSDate)
import Data.JSDate (now) as JSDate
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Microtime (microtime)
import Util (type (×), (×))

newtype File = File String
newtype Folder = Folder String

derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

data BenchRow = BenchRow TraceRow GraphRow

newtype BenchAcc = BenchAcc (Array (String × BenchRow))

type WithBenchAcc g a = WriterT BenchAcc g a

runWithBenchAcc :: forall g a. Monad g => WithBenchAcc g a -> g (a × BenchAcc)
runWithBenchAcc = runWriterT

derive newtype instance Semigroup BenchAcc
derive newtype instance Monoid BenchAcc

type TraceRow =
   { tEval :: Number
   , tBwd :: Number
   , tFwd :: Number
   }

type GraphRow =
   { tEval :: Number
   , tBwd :: Number
   , tBwdDual :: Number
   , tBwdAll :: Number
   , tFwd :: Number
   , tFwdDual :: Number
   , tFwdAsDemorgan :: Number
   }

instance Show BenchAcc where
   show (BenchAcc rows) =
      "Test-Name, Trace-Eval, Trace-Bwd, Trace-Fwd, Graph-Eval, Graph-Bwd, Graph-BwdDual, Graph-BwdAll, Graph-Fwd, Graph-FwdDual, Graph-FwdAsDeMorgan\n"
         <> (fold $ intersperse "\n" (map rowShow rows))

rowShow :: String × BenchRow -> String
rowShow (str × row) = str <> "," <> show row

zeroRow :: BenchRow
zeroRow =
   BenchRow { tEval: 0.0, tBwd: 0.0, tFwd: 0.0 }
      { tEval: 0.0, tBwd: 0.0, tBwdDual: 0.0, tBwdAll: 0.0, tFwd: 0.0, tFwdDual: 0.0, tFwdAsDemorgan: 0.0 }

sumRow :: BenchRow -> BenchRow -> BenchRow
sumRow (BenchRow trRow1 gRow1) (BenchRow trRow2 gRow2) =
   BenchRow
      { tEval: trRow1.tEval + trRow2.tEval
      , tBwd: trRow1.tBwd + trRow2.tBwd
      , tFwd: trRow1.tFwd + trRow2.tFwd
      }
      { tEval: gRow1.tEval + gRow2.tEval
      , tBwd: gRow1.tBwd + gRow2.tBwd
      , tBwdDual: gRow1.tBwdDual + gRow2.tBwdDual
      , tBwdAll: gRow1.tBwdAll + gRow2.tBwdAll
      , tFwd: gRow1.tFwd + gRow2.tFwd
      , tFwdDual: gRow1.tFwdDual + gRow2.tFwdDual
      , tFwdAsDemorgan: gRow1.tFwdAsDemorgan + gRow2.tFwdAsDemorgan
      }

instance Show BenchRow where
   show (BenchRow trRow grRow) = fold $ intersperse ","
      [ show trRow.tEval
      , show trRow.tBwd
      , show trRow.tFwd
      , show grRow.tEval
      , show grRow.tBwd
      , show grRow.tBwdDual
      , show grRow.tBwdAll
      , show grRow.tFwd
      , show grRow.tFwdDual
      , show grRow.tFwdAsDemorgan
      ]

now :: forall m. MonadEffect m => m JSDate
now = liftEffect JSDate.now

tdiff :: Number -> Number -> Number
tdiff x y = sub y x

preciseTime :: forall m. MonadEffect m => m Number
preciseTime = liftEffect microtime