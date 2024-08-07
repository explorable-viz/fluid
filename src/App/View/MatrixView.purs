module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (ReactState, SelState, Selectable, 𝕊, ViewSelector, Relectable)
import App.Util.Selector (matrixElement)
import App.View.Util (class Drawable, Renderer, RRenderer, selListener, uiHelpers, uiRHelpers)
import Primitive (int, unpack)
import Util ((×))
import Val (Array2, MatrixRep(..))

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = { cells :: Array2 (Selectable Int), i :: Int, j :: Int }

type RIntMatrix = { cells :: Array2 (Relectable Int), i :: Int, j :: Int }

newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

newtype RMatrixView = RMatrixView { title :: String, matrix :: RIntMatrix }

foreign import drawMatrix :: Renderer MatrixView Unit

foreign import drawRMatrix :: RRenderer RMatrixView Unit

instance Drawable MatrixView Unit where
   draw divId suffix redraw view viewState =
      drawMatrix { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw matrixViewSelector
      where
      matrixViewSelector :: ViewSelector MatrixCellCoordinate
      matrixViewSelector { i, j } = matrixElement i j

matrixRep :: MatrixRep (SelState 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

instance Drawable RMatrixView Unit where
   draw divId suffix redraw view viewState =
      drawRMatrix { uiRHelpers, divId, suffix, view, viewState } =<< selListener redraw matrixViewSelector
      where
      matrixViewSelector :: ViewSelector MatrixCellCoordinate
      matrixViewSelector { i, j } = matrixElement i j

matrixRRep :: MatrixRep (ReactState 𝕊) -> RIntMatrix
matrixRRep (MatrixRep (vss × (i × _) × (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

-- 1-based indices of selected cell; see data binding in .js
type MatrixCellCoordinate = { i :: Int, j :: Int }
