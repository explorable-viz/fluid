module App.MatrixView where

import Prelude hiding (absurd)

import App.Util (Handler, Renderer)
import App.Util.Select (matrixElement)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Lattice (𝔹, neg)
import Primitive (int)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (×), (!), definitely')
import Val (Array2, MatrixRep(..))
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: MatrixRep 𝔹 -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   ((int.unpack <$> _) <$> vss) × i × j

matrixViewHandler :: Handler
matrixViewHandler = target >>> unsafePos >>> flip (uncurry matrixElement) neg
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   unsafePos :: Maybe EventTarget -> Int × Int
   unsafePos tgt_opt = xy ! 0 × xy ! 1
      where
      xy = (unsafeCoerce $ definitely' tgt_opt).__data__ ! 0 :: Array Int
