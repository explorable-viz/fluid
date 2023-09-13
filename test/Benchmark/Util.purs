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
import Data.JSDate (JSDate, getTime, now)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (logShow, log)
import Util (MayFailT, error, type (×), (×))

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
   show (BenchAcc rows) = "Test-Name, Trace-Eval, Trace-Bwd, Trace-Fwd, Graph-Eval, Graph-Bwd, Graph-Fwd, Graph-DeMorgan\n" <> (fold $ intersperse "\n" (map rowShow rows))

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

bench :: forall m a. MonadEffect m => m a -> m (a × Number)
bench prog = do
   t <- liftEffect $ now
   r <- prog
   t' <- liftEffect $ now
   pure (r × timeDiff t t')

getCurr :: forall m. MonadEffect m => m JSDate
getCurr = liftEffect $ now

timeDiff :: JSDate -> JSDate -> Number
timeDiff begin end =
   getTime end - getTime begin

logTime :: String -> JSDate -> JSDate -> Aff Unit
logTime msg before after =
   liftEffect $ log (msg <> show (timeDiff before after) <> "\n")

-- liftTimer :: Test Unit -> Test Unit
-- liftTimer test = do
--    (begin :: ?_ )<- lift now
--    out <- test
--    end <- lift now
--    lift $ logShow (timeDiff begin end)
--    pure out

liftMayFail :: MayFailT Aff Unit -> MayFailT Aff Unit
liftMayFail test = do
   begin <- liftEffect $ now
   test
   end <- liftEffect $ now
   liftEffect $ logShow (timeDiff begin end)

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
