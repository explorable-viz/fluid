module App.Renderer where

import Prelude
import Data.Function.Uncurried (Fn2)
import Lattice (𝔹)
import Util ((×))
import Val (MatrixRep)

foreign import drawMatrix :: Fn2 Number Number Unit

renderMatrix :: MatrixRep 𝔹 -> Unit
renderMatrix (vss × (i × _) × (j × _)) = unit
