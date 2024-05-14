module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (Handler, Renderer, 𝕊, Selectable, unsafeEventData)
import App.Util.Selector (matrixElement)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import Lattice (neg)
import Primitive (int, unpack)
import Util (type (×), (!), (×))
import Val (Array2, MatrixRep(..))
import Web.Event.Event (EventType, target, type_)
import Web.Event.EventTarget (EventTarget)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Selectable Int) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: MatrixRep 𝕊 -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   ((unpack int <$> _) <$> vss) × i × j

matrixViewHandler :: Handler
matrixViewHandler = (target &&& type_) >>> pos >>> flip (uncurry matrixElement) neg
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   pos :: Maybe EventTarget × EventType -> Int × Int
   pos (tgt_opt × _) = xy ! 0 × xy ! 1
      where
      xy = unsafeEventData tgt_opt ! 0 :: Array Int
