module Benchmark.Timers where

import Prelude
import Effect.Console (logShow)
import Control.Monad.Trans.Class (lift)
import Data.JSDate (JSDate, getTime, now)
import Test.Util (Test)

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
