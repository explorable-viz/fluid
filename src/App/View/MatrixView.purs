module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (SelState, Selectable, 𝕊, ViewSelector)
import App.Util.Selector (matrixElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Array (concat, drop, filter, head, last, length, mapWithIndex, replicate, snoc, tail, take, updateAt, zip, zipWith, (!!), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested (Tuple3, get3, tuple3)
import Foreign.Object (Object, fromFoldable)
import Partial.Unsafe (unsafePartial)
import Primitive (int, unpack)
import Util ((×))
import Val (MatrixRep(..), Array2)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = { cells :: Array2 (Selectable Int), i :: Int, j :: Int }
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

type MatrixHelpers =
   { hBorder_attrs :: IntMatrix -> MatrixCellCoordinate -> Object String
   , vBorder_attrs :: IntMatrix -> MatrixCellCoordinate -> Object String
   }

hBorder_attrs :: IntMatrix -> MatrixCellCoordinate -> Object String
hBorder_attrs _ _ = fromFoldable []

vBorder_attrs :: IntMatrix -> MatrixCellCoordinate -> Object String
vBorder_attrs _ _ = fromFoldable []

foreign import drawMatrix :: MatrixHelpers -> Renderer MatrixView Unit

drawMatrix' :: Renderer MatrixView Unit
drawMatrix' = drawMatrix
   { hBorder_attrs
   , vBorder_attrs
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

getHBordersToHighlight :: Array2 Boolean -> Array2 Boolean
getHBordersToHighlight cells =
   let
      zippedRows = zip (take (length cells - 1) cells) (drop 1 cells)
      midRows = map (\(r1 × r2) -> zipWith (/=) r1 r2) zippedRows
   in
      (unsafePartial fromJust $ head cells) : midRows `snoc` (unsafePartial fromJust $ last cells)

getVBordersToHighlight :: Array2 Boolean -> Array2 Boolean
getVBordersToHighlight = map
   ( \row ->
        let
           midBorders = zipWith (/=) (take (length row - 1) row) (drop 1 row)
        in
           (unsafePartial fromJust $ head row) : midBorders `snoc` (unsafePartial fromJust $ last row)
   )

getCellsToHighlight :: IntMatrix -> MatrixCellCoordinate -> Int -> Int -> Array2 Boolean
getCellsToHighlight filter mousePos matrixH matrixW =
   let
      blank = replicate matrixH $ replicate matrixW false
      activeFilterCells = getActiveCellsWithCoords filter
   in
      _getCellsToHighlight activeFilterCells mousePos matrixH matrixW blank

_getCellsToHighlight :: forall a. Array (Tuple3 Int Int a) -> MatrixCellCoordinate -> Int -> Int -> Array2 Boolean -> Array2 Boolean
_getCellsToHighlight filterCells mousePos@{ i: mouseY, j: mouseX } matrixH matrixW matrix =
   case head filterCells of
      Just (cellY × cellX × _) ->
         let
            newY = (mouseY + cellY) `mod` matrixH
            newX = (mouseX + cellX) `mod` matrixW
         in
            _getCellsToHighlight (unsafePartial fromJust $ tail filterCells) mousePos matrixH matrixW (updateAt2 matrix newY newX true)
      Nothing -> matrix

-- get a list of (yCoord, xCoord, cellVal) tuples containing non-zero cells in the matrix
getActiveCellsWithCoords :: IntMatrix -> Array (Tuple3 Int Int Int)
getActiveCellsWithCoords { cells, i: filterH, j: filterW } =
   let
      yShift = filterH / 2
      xShift = filterW / 2
      cellsVal = map (map (\(n × _) -> n)) cells -- from Array2 (Selectable Int) to Array2 Int
      -- attach to each cell its coordinate relative to the center of the filter matrix
      tuples2d = mapWithIndex (\y row -> mapWithIndex (\x n -> tuple3 (y - yShift) (x - xShift) n) row) cellsVal
   in
      filter (\t -> (get3 t) /= 0) $ concat $ tuples2d

updateAt2 :: forall a. Array2 a -> Int -> Int -> a -> Array2 a
updateAt2 arr2d y x val =
   let
      newRow = unsafePartial fromJust $ updateAt x val (unsafePartial fromJust $ arr2d !! y)
   in
      unsafePartial fromJust $ updateAt y newRow arr2d
