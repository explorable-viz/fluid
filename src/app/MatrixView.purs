module App.MatrixView where

import Prelude
import Data.Array (zip, zipWith)
import Effect (Effect)
import App.Util (HTMLId)
import Lattice (𝔹)
import Primitive (Slice, match_fwd)
import Util (type (×), (×))
import Val (Array2, MatrixRep)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: HTMLId -> MatrixView -> Effect Unit

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) (zipWith zip vss uss) × i × j
