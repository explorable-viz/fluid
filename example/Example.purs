module Example.Example where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Example.Util.BMA (IntInf(..))
import Example.Util.DTW (distEuclid, distanceDTWWindow)
import Partial.Unsafe (unsafePartial)
import Util ((×))
main :: Effect Unit
main = do
    log "Beginning DTW!"
    logShow $ distEuclid (IInt 4) (IInt 2)
    logShow $ min (IInt 4) Infty
    let x = [IInt 3, IInt 1, IInt 2, IInt 2, IInt 1]
        y = [IInt 2, IInt 0, IInt 0, IInt 3, IInt 3, IInt 1, IInt 0]
        m1 × m2 = unsafePartial $ distanceDTWWindow x y 7 distEuclid 
    logShow m2
    logShow m1
