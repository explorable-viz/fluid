module Example.Example where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Example.Util.BMA (mean)
import Data.FastVect.FastVect ((:), empty)

main :: Effect Unit
main = do
    logShow "100"
    logShow (mean 2.0 (1.0:2.0:3.0:4.0:5.0:6.0:7.0:8.0:9.0:10.0: empty))