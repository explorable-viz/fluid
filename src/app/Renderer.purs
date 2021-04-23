module App.Renderer where

import Prelude
import Lattice (𝔹)
import Util ((×))
import Val (MatrixRep)

renderMatrix :: MatrixRep 𝔹 -> Unit
renderMatrix (vss × (i × _) × (j × _)) = ?_
