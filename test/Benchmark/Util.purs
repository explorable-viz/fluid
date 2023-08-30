module Benchmark.Util where

import Prelude

import Affjax.RequestBody (string) as RB
import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.JSDate (JSDate, getTime, now)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (logShow)
import Test.Spec (SpecT)
import Util (MayFailT, error)

newtype File = File String
newtype Folder = Folder String

derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File
type Test a = SpecT Aff Unit Effect a

type BenchResult =
   { name :: String
   , desug :: Number
   , trace_eval :: Number
   , graph_eval :: Number
   , trace_fwd :: Number
   , trace_bwd :: Number
   , graph_fwd :: Number
   , graph_bwd :: Number
   }

type BenchSet = Array BenchResult

getCurr :: forall a. MonadEffect a => a JSDate
getCurr = liftEffect $ now

timeDiff :: JSDate -> JSDate -> Number
timeDiff begin end =
   getTime end - getTime begin

liftTimer :: Test Unit -> Test Unit
liftTimer test = do
   begin <- lift now
   out <- test
   end <- lift now
   lift $ logShow (timeDiff begin end)
   pure out

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