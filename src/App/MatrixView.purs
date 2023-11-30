module App.MatrixView where

import Prelude hiding (absurd)

import App.Util (Handler, Renderer, Sel)
import App.Util.Select (matrixElement)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Lattice (neg)
import Primitive (int, unpack)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (×), (!), definitely')
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
matrixViewHandler = target >>> unsafePos >>> flip (uncurry matrixElement) neg
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   unsafePos :: Maybe EventTarget -> Int × Int
   unsafePos tgt_opt = xy ! 0 × xy ! 1
      where
      xy = (unsafeCoerce $ definitely' tgt_opt).__data__ ! 0 :: Array Int
