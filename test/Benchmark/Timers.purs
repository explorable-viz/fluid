module Benchmark.Timers where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.JSDate (JSDate, getTime, now)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Test.Spec (SpecT)
import Util (MayFailT)

type Test a = SpecT Aff Unit Effect a

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