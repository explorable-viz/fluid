module Benchmark.Util where

import Prelude

import Affjax.RequestBody (string) as RB
import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (intersperse)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.HTTP.Method (Method(..))
import Data.JSDate (JSDate, getTime)
import Data.JSDate (now) as JSDate
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Util (error, type (×), (×))

newtype File = File String
newtype Folder = Folder String

derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File
-- type Test a = SpecT Aff Unit BenchmarkAcc a

data BenchRow = BenchRow TraceRow GraphRow

data BenchAcc = BenchAcc (Array (String × BenchRow))

type WithBenchAcc g a = WriterT BenchAcc g a

runWithBenchAcc :: forall g a. Monad g => WithBenchAcc g a -> g (a × BenchAcc)
runWithBenchAcc = runWriterT

instance Semigroup BenchAcc where
   append (BenchAcc l1) (BenchAcc l2) = (BenchAcc (l1 <> l2))

instance Monoid BenchAcc where
   mempty = BenchAcc ([])

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
   show (BenchAcc rows) = "Test-Name, Trace-Eval, Trace-Bwd, Trace-Fwd, Graph-Eval, Graph-Bwd, Graph-Fwd, Graph-FwdDeMorgan\n" <> (fold $ intersperse "\n" (map rowShow rows))

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
now = liftEffect $ JSDate.now

tdiff :: JSDate -> JSDate -> Number
tdiff begin end =
   getTime end - getTime begin

loadBenches :: Aff String
loadBenches = do
   let url = "./Benchmark/benchmarks.csv"
   result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> error (printError err)
      Right response -> pure response.body

appendLine :: String -> Aff Unit
appendLine line = do
   old <- loadBenches
   let
      url = ".Benchmark/benchmarks.csv"
      new = old <> "\n" <> line
   result <- request (defaultRequest { content = Just (RB.string new), url = url, method = Left PUT })
   case result of
      Left err -> error (printError err)
      Right response -> pure response.body
