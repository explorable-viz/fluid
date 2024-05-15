module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (Handler, Renderer, Selectable, Selector, 𝕊, SelState, selector, unsafeEventData)
import App.Util.Selector (matrixElement)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import Primitive (int, unpack)
import Util (type (×), (!), (×))
import Val (Array2, MatrixRep(..), Val)
import Web.Event.Event (EventType, target, type_)
import Web.Event.EventTarget (EventTarget)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Selectable Int) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: MatrixRep (SelState 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   ((unpack int <$> _) <$> vss) × i × j

matrixViewHandler :: Handler
matrixViewHandler = (target &&& type_) >>> pos >>> uncurry (uncurry matrixElement)
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   pos :: Maybe EventTarget × EventType -> (Int × Int) × Selector Val
   pos (tgt_opt × ty) = (xy ! 0 × xy ! 1) × selector ty
      where
      xy = unsafeEventData tgt_opt ! 0 :: Array Int
