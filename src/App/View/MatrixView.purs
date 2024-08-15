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

data ShadowDirection = North | South | East | West | None

matrixBorderStyles :: ShadowDirection -> String
matrixBorderStyles North = "filter: drop-shadow(0px -1px 1px blue);"
matrixBorderStyles South = "filter: drop-shadow(0px 1px 1px blue);"
matrixBorderStyles East = "filter: drop-shadow(1px 0px 1px blue);"
matrixBorderStyles West = "filter: drop-shadow(-1px 0px 1px blue);"
matrixBorderStyles None = "visibility: hidden;"

hBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
hBorderStyles m = matrixBorderStyles <<< hBorderShadowDirection m

hBorderShadowDirection :: IntMatrix -> MatrixBorderCoordinate -> ShadowDirection
hBorderShadowDirection { cells, i: height } { i, j }
   | i == 0 = if isCellTransient cells { i, j: j - 1 } then South else None
   | i == height = if isCellTransient cells { i: i - 1, j: j - 1 } then North else None
   | otherwise =
        if isCellTransient cells { i, j: j - 1 } && (not isCellTransient cells { i: i - 1, j: j - 1 }) then South
        else if (not isCellTransient cells { i, j: j - 1 }) && isCellTransient cells { i: i - 1, j: j - 1 } then North
        else None

vBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
vBorderStyles m = matrixBorderStyles <<< vBorderShadowDirection m

vBorderShadowDirection :: IntMatrix -> MatrixBorderCoordinate -> ShadowDirection
vBorderShadowDirection { cells, j: width } { i, j }
   | j == 0 = if isCellTransient cells { i: i - 1, j } then East else None
   | j == width = if isCellTransient cells { i: i - 1, j: j - 1 } then West else None
   | otherwise =
        if isCellTransient cells { i: i - 1, j } && not isCellTransient cells { i: i - 1, j: j - 1 } then East
        else if not isCellTransient cells { i: i - 1, j } && isCellTransient cells { i: i - 1, j: j - 1 } then West
        else None

isCellTransient :: forall a. Array2 (Selectable a) -> MatrixCellCoordinate -> Boolean
isCellTransient arr2d { i, j } = isTransient $ snd $ arr2d ! i ! j

foreign import drawMatrix :: MatrixViewHelpers -> Renderer MatrixView Unit

type MatrixViewHelpers =
   { hBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
   , vBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
   }

drawMatrix' :: Renderer MatrixView Unit
drawMatrix' = drawMatrix
   { hBorderStyles
   , vBorderStyles
   }

instance Drawable MatrixView Unit where
   draw divId suffix redraw view viewState =
      drawMatrix' uiHelpers { divId, suffix, view, viewState } =<< selListener redraw matrixViewSelector
      where
      matrixViewSelector :: ViewSelector MatrixCellCoordinate
      matrixViewSelector { i, j } = matrixElement i j

matrixRep :: MatrixRep (SelState 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

-- 1-based indices of selected cell; see data binding in .js
type MatrixCellCoordinate = { i :: Int, j :: Int }

type MatrixBorderCoordinate = { i :: Int, j :: Int }
