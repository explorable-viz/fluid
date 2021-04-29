module App.Renderer where

import Prelude
import Data.Function.Uncurried (Fn2)
import Effect (Effect)
import Lattice (𝔹)
import Util ((×))
import Val (MatrixRep)

foreign import drawMatrix :: Fn2 Number Number (Effect Unit)

renderMatrix :: MatrixRep 𝔹 -> Effect Unit
renderMatrix (vss × (i × _) × (j × _)) = pure unit
