module Standalone.Website where

import Prelude hiding (absurd)

import App.Fig (drawFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)

main :: Effect Unit
main = runAffs_ (uncurry drawFig)
   [
   ]
