module App.Renderer where

import Prelude
import Effect (Effect)
import Lattice (𝔹)
import Util (type (×), (×))
import Val (MatrixRep)

foreign import drawMatrix :: Int -> Int -> Effect Unit

renderMatrix :: MatrixRep 𝔹 × 𝔹 -> Effect Unit
renderMatrix (_ × (i × _) × (j × _) × _) = drawMatrix i j
