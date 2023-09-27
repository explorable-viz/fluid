module Example.Example where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Example.Util.BMA (IntInf(..))
import Example.Util.DTW (distEuclid, distanceDTW)

main :: Partial => Effect Unit
main = do
    let x = [IInt 3, IInt 1, IInt 2, IInt 2, IInt 1]
        y = [IInt 2, IInt 0, IInt 0, IInt 3, IInt 3, IInt 1, IInt 0]
        dtwMat = distanceDTW x y 1 distEuclid
    logShow dtwMat
