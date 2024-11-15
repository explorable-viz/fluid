module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (SelState, Selectable, 𝕊, isTransient)
import App.Util.Selector (ViewSelSetter, matrixElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Tuple (snd)
import Primitive (int, unpack)
import Util ((!), (×))
import Val (MatrixRep(..), Array2)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = { cells :: Array2 (Selectable Int), i :: Int, j :: Int }

newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: MatrixViewHelpers -> Renderer MatrixView

type MatrixViewHelpers =
   { hBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
   , vBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
   }

data ShadowDirection = North | South | East | West | None

matrixViewHelpers :: MatrixViewHelpers
matrixViewHelpers =
   { hBorderStyles
   , vBorderStyles
   }
   where
   hBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
   hBorderStyles m = borderStyles <<< shadowDirection m
      where
      shadowDirection :: IntMatrix -> MatrixBorderCoordinate -> ShadowDirection
      shadowDirection { cells, i: height } { i, j }
         | i == 0 = if isCellTransient cells i (j - 1) then South else None
         | i == height = if isCellTransient cells (i - 1) (j - 1) then North else None
         | isCellTransient cells i (j - 1) && not isCellTransient cells (i - 1) (j - 1) = South
         | not isCellTransient cells i (j - 1) && isCellTransient cells (i - 1) (j - 1) = North
         | otherwise = None

   vBorderStyles :: IntMatrix -> MatrixBorderCoordinate -> String
   vBorderStyles m = borderStyles <<< shadowDirection m
      where
      shadowDirection :: IntMatrix -> MatrixBorderCoordinate -> ShadowDirection
      shadowDirection { cells, j: width } { i, j }
         | j == 0 = if isCellTransient cells (i - 1) j then East else None
         | j == width = if isCellTransient cells (i - 1) (j - 1) then West else None
         | isCellTransient cells (i - 1) j && not isCellTransient cells (i - 1) (j - 1) = East
         | not isCellTransient cells (i - 1) j && isCellTransient cells (i - 1) (j - 1) = West
         | otherwise = None

   isCellTransient :: forall a. Array2 (Selectable a) -> Int -> Int -> Boolean
   isCellTransient arr2d i j = isTransient $ snd $ arr2d ! i ! j

   borderStyles :: ShadowDirection -> String
   borderStyles North = "filter: drop-shadow(0px -1px 1px blue);"
   borderStyles South = "filter: drop-shadow(0px 1px 1px blue);"
   borderStyles East = "filter: drop-shadow(1px 0px 1px blue);"
   borderStyles West = "filter: drop-shadow(-1px 0px 1px blue);"
   borderStyles None = "visibility: hidden;"

instance Drawable MatrixView where
   draw rSpec figVal _ redraw =
      drawMatrix matrixViewHelpers uiHelpers rSpec =<< selListener figVal redraw element
      where
      element :: ViewSelSetter MatrixCellCoordinate
      element { i, j } = matrixElement i j

matrixRep :: MatrixRep (SelState 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × (i × _) × (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

-- 1-based indices of selected cell; see data binding in .js
type MatrixCellCoordinate = { i :: Int, j :: Int }
type MatrixBorderCoordinate = { i :: Int, j :: Int }
