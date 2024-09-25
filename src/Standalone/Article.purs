module Standalone.Article where

import Prelude hiding (absurd)

import App.Fig (drawFig, loadFig)
import App.Util (runAffs_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Test.Specs.LinkedInputs (energyScatter)
import Util ((×))

-- This will evolve into the "generic" Article entry point once we can load a FigSpec from a .yml file.
-- For now just load a specific figure.

main :: Effect Unit
main = runAffs_ (uncurry drawFig) [ ("fig" × _) <$> loadFig energyScatter ]
