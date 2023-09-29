module Example.Example where

import Prelude

import Data.Array ((..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Example.Util.DTW (distEuclid, distanceDTWWindow)
import Partial.Unsafe (unsafePartial)
import Util ((×))

main :: Effect Unit
main = do
   log "Beginning DTW!"
   let
      n = 5
      m = 7
      window = 2
      nextIndices = do
         i <- 1 .. n
         j <- (max 1 (i - window)) .. (min m (i + window))
         [ (i × j) ]
   logShow nextIndices
   let
      x = [ 3.0, 1.0, 2.0, 2.0, 1.0 ]
      y = [ 2.0, 0.0, 0.0, 3.0, 3.0, 1.0, 0.0 ]
      m1 × m2 = unsafePartial $ distanceDTWWindow x y 2 distEuclid
   log "Finished DTW"
   logShow m2
   logShow m1
