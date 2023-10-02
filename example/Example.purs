module Example.Example where

import Prelude

import Data.Array ((..))
import Data.List ((:), List(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Example.Util.DTW (costMatrixInit, distEuclid, distanceDTWWindow)
import Partial.Unsafe (unsafePartial)
import Util ((×))

main :: Effect Unit
main = do
   log "Beginning DTW!"
   let
      n = 5
      m = 7
      window = 2
      initMat = costMatrixInit 5 7 2
   logShow initMat
   let
      x = (3.0 : 1.0 : 2.0 : 2.0 : 1.0 : Nil)
      y = (2.0 : 0.0 : 0.0 : 3.0 : 3.0 : 1.0 : 0.0 : Nil)
      m1 × m2 = unsafePartial $ distanceDTWWindow x y 2 distEuclid
   log "Finished DTW"
   logShow m2
   logShow m1
