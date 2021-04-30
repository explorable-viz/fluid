module App.Renderer where

import Prelude
import Data.Tuple (fst)
import Effect (Effect)
import Lattice (𝔹)
import Primitive (match)
import Util (type (×), (×))
import Val (MatrixRep, Val)

foreign import drawMatrix :: Array (Array Number) -> Int -> Int -> Effect Unit

-- Will want to generalise to arrays of "drawable values".
toNumArray :: Array (Array (Val 𝔹)) -> Array (Array Number)
toNumArray = (<$>) ((<$>) (match >>> fst))

renderMatrix :: MatrixRep 𝔹 × 𝔹 -> Effect Unit
renderMatrix (vss × (i × _) × (j × _) × _) = drawMatrix (toNumArray vss) i j
