module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (SelState, Selectable, ViewSelector, 𝕊, isTransient)
import App.Util.Selector (matrixElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Tuple (snd)
import Primitive (int, unpack)
import Util ((!), (×))
import Val (MatrixRep(..), Array2)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = { cells :: Array2 (Selectable Int), i :: Int, j :: Int }
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

type MatrixHelpers =
   { isHBorderExterior :: IntMatrix -> MatrixBorderCoordinate -> Boolean
   , isVBorderExterior :: IntMatrix -> MatrixBorderCoordinate -> Boolean
   }

-- horizontal border: i ∈ [0..matrixH], j ∈ [1..matrixW]
isHBorderExterior :: IntMatrix -> MatrixBorderCoordinate -> Boolean
isHBorderExterior { cells, i: matrixH, j: _ } { i, j }
   | i == 0 = isCellTransient cells { i, j: j - 1 } -- top border of the entire matrix, check only the cell below
   | i == matrixH = isCellTransient cells { i: i - 1, j: j - 1 } -- bottom border of the entire matrix, check only the cell above
   | otherwise = isCellTransient cells { i, j: j - 1 } /= isCellTransient cells { i: i - 1, j: j - 1 }

-- vertical border: i ∈ [1..matrixH], j ∈ [0..matrixW]
isVBorderExterior :: IntMatrix -> MatrixBorderCoordinate -> Boolean
isVBorderExterior { cells, i: _, j: matrixW } { i, j }
   | j == 0 = isCellTransient cells { i: i - 1, j } -- left border of the entire matrix, check only the cell to the right
   | j == matrixW = isCellTransient cells { i: i - 1, j: j - 1 } -- right border of the entire matrix, check only the cell to the left
   | otherwise = isCellTransient cells { i: i - 1, j } /= isCellTransient cells { i: i - 1, j: j - 1 }

isCellTransient :: forall a. Array2 (Selectable a) -> MatrixCellCoordinate -> Boolean
isCellTransient arr2d { i, j } = isTransient $ snd $ (arr2d ! i) ! j

foreign import drawMatrix :: MatrixHelpers -> Renderer MatrixView Unit

drawMatrix' :: Renderer MatrixView Unit
drawMatrix' = drawMatrix
   { isHBorderExterior
   , isVBorderExterior
   }

instance Drawable MatrixView Unit where
   draw divId suffix redraw view viewState =
      drawMatrix' { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw matrixViewSelector
      where
      matrixViewSelector :: ViewSelector MatrixCellCoordinate
      matrixViewSelector { i, j } = matrixElement i j

matrixRep :: MatrixRep (SelState 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

-- 1-based indices of selected cell; see data binding in .js
type MatrixCellCoordinate = { i :: Int, j :: Int }

type MatrixBorderCoordinate = { i :: Int, j :: Int }
