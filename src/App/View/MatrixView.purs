module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (Handler, Renderer, Selectable, Selector, 𝕊, SelState, selector, unsafeEventData)
import App.Util.Selector (matrixElement)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import Primitive (int, unpack)
import Util (type (×), (×))
import Val (Array2, MatrixRep(..), Val)
import Web.Event.Event (EventType, target, type_)
import Web.Event.EventTarget (EventTarget)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = { cells :: Array2 (Selectable Int), i :: Int, j :: Int }
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: MatrixRep (SelState 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

-- see data binding in .js
type MatrixCellCoordinate = { i :: Int, j :: Int }

matrixViewHandler :: Handler
matrixViewHandler = (target &&& type_) >>> pos >>> uncurry \{ i, j } -> matrixElement i j
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   pos :: Maybe EventTarget × EventType -> MatrixCellCoordinate × Selector Val
   pos (tgt_opt × ty) = unsafeEventData tgt_opt × selector ty
