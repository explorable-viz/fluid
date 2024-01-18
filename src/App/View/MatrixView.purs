module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (Handler, Renderer, Sel, unsafeEventData)
import App.Util.Selector (matrixElement)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Lattice (neg)
import Primitive (int, unpack)
import Util (type (×), (!), (×))
import Val (Array2, MatrixRep(..))
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × Sel) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: MatrixRep Sel -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   ((unpack int <$> _) <$> vss) × i × j

matrixViewHandler :: Handler
matrixViewHandler = target >>> pos >>> flip (uncurry matrixElement) neg
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   pos :: Maybe EventTarget -> Int × Int
   pos tgt_opt = xy ! 0 × xy ! 1
      where
      xy = unsafeEventData tgt_opt ! 0 :: Array Int
