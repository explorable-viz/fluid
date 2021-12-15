module App.DrawMatrix where

import Prelude
import Effect (Effect)
import App.Util (HTMLId)
import Lattice (𝔹)
import Util (type (×))
import Val (Array2)

type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: HTMLId -> MatrixView -> Effect Unit
