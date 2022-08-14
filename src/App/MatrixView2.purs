module App.MatrixView2 where

import Prelude hiding (absurd)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util2 (Handler, Renderer, toggleCell)
import Lattice2 (𝔹)
import Primitive2 (match_fwd)
import Util2 (type (×), (×), (!), absurd, fromJust)
import Val2 (Array2, MatrixRep)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: MatrixRep 𝔹 -> IntMatrix
matrixRep ((vss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) vss × i × j

matrixViewHandler :: Handler
matrixViewHandler ev = uncurry toggleCell $ unsafePos $ target ev
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   unsafePos :: Maybe EventTarget -> Int × Int
   unsafePos tgt_opt =
      let tgt = fromJust absurd $ tgt_opt
          xy = (unsafeCoerce tgt).__data__!0 :: Array Int
      in xy!0 × xy!1
