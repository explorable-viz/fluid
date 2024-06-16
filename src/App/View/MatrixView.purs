module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (SelState, Selectable, 𝕊, ViewSelector)
import App.Util.Selector (matrixElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Primitive (int, unpack)
import Util ((×))
import Val (Array2, MatrixRep(..))

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = { cells :: Array2 (Selectable Int), i :: Int, j :: Int }
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

instance Drawable MatrixView Unit where
   initialState _ = unit
   draw divId suffix redraw vw _ =
      drawMatrix { uiHelpers, divId, suffix, view: vw } =<< selListener redraw matrixViewSelector

matrixRep :: MatrixRep (SelState 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

-- 1-based indices of selected cell; see data binding in .js
type MatrixCellCoordinate = { i :: Int, j :: Int }

matrixViewSelector :: ViewSelector MatrixCellCoordinate
matrixViewSelector { i, j } = matrixElement i j
