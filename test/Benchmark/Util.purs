module Benchmark.Util where

import Prelude

import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (intersperse)
import Data.Foldable (fold)
import Data.JSDate (JSDate, getTime)
import Data.JSDate (now) as JSDate
import Effect.Class (class MonadEffect, liftEffect)
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
   , tFwd :: Number
   , tFwdDemorgan :: Number
   }

instance Show BenchAcc where
   show (BenchAcc rows) =
      "Test-Name, Trace-Eval, Trace-Bwd, Trace-Fwd, Graph-Eval, Graph-Bwd, Graph-Fwd, Graph-DeMorgan\n" <>
         (fold $ intersperse "\n" (rowShow <$> rows))

rowShow :: String × BenchRow -> String
rowShow (str × row) = str <> "," <> show row

instance Show BenchRow where
   show (BenchRow trRow grRow) = fold $ intersperse ","
      [ show trRow.tEval
      , show trRow.tBwd
      , show trRow.tFwd
      , show grRow.tEval
      , show grRow.tBwd
      , show grRow.tFwd
      , show grRow.tFwdDemorgan
      ]

now :: forall m. MonadEffect m => m JSDate
now = liftEffect JSDate.now

tdiff :: JSDate -> JSDate -> Number
tdiff begin end =
   getTime end - getTime begin
