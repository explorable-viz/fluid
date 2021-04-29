module App.Renderer where

import Prelude
import Effect (Effect)
import Lattice (𝔹)
import Util ((×))
import Val (MatrixRep)

foreign import drawMatrix :: Int -> Int -> Effect Unit

renderMatrix :: MatrixRep 𝔹 -> Effect Unit
renderMatrix (vss × (i × _) × (j × _)) = pure unit
