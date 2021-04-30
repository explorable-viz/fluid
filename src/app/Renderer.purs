module App.Renderer where

import Prelude
import Effect (Effect)
import Lattice (𝔹)
import Primitive (match)
import Util (type (×), (×))
import Val (MatrixRep, Val)

foreign import drawMatrix :: Array (Array (Int × 𝔹)) -> Int -> Int -> Effect Unit

-- Will want to generalise to arrays of "drawable values".
toIntArray :: Array (Array (Val 𝔹)) -> Array (Array (Int × 𝔹))
toIntArray = (<$>) ((<$>) match)

renderMatrix :: MatrixRep 𝔹 × 𝔹 -> Effect Unit
renderMatrix (vss × (i × _) × (j × _) × _) = drawMatrix (toIntArray vss) i j
