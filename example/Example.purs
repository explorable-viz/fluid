module Example.Example where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Example.Util.DTW (NumInf(..), distEuclid, distanceDTWWindow)
import Partial.Unsafe (unsafePartial)
import Util ((×))

main :: Effect Unit
main = do
   log "Beginning DTW!"
   let
      x = [ FNum 3.0, FNum 1.0, FNum 2.0, FNum 2.0, FNum 1.0 ]
      y = [ FNum 2.0, FNum 0.0, FNum 0.0, FNum 3.0, FNum 3.0, FNum 1.0, FNum 0.0 ]
      m1 × m2 = unsafePartial $ distanceDTWWindow x y 7 distEuclid
   logShow m2
   logShow m1
