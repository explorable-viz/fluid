module App.MatrixView where

import Prelude hiding (absurd)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util (Handler, Renderer, toggleCell)
import Lattice (𝔹)
import Primitive (match)
import Util (type (×), (×), (!), definitely')
import Val (Array2, MatrixRep)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: MatrixRep 𝔹 -> IntMatrix
matrixRep ((vss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match)) vss × i × j

matrixViewHandler :: Handler
matrixViewHandler ev = uncurry toggleCell $ unsafePos $ target ev
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   unsafePos :: Maybe EventTarget -> Int × Int
   unsafePos tgt_opt =
      let tgt = definitely' $ tgt_opt
          xy = (unsafeCoerce tgt).__data__!0 :: Array Int
      in xy!0 × xy!1
