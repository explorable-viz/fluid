module App.View.MatrixView where

import Prelude hiding (absurd)

import App.Util (SelStates, Selectable, 𝕊, isTransient, selectionEventData')
import App.Util.Selector (ViewSelSetter, matrixElement)
import App.View.Util (class Viewable, Select, UIHelpers, uiHelpers)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Tuple (snd, uncurry)
import Effect (Effect, foreachE)
import Primitive (int, unpack)
import Util ((!), (×))
import Val (Array2, MatrixDim(..), MatrixRep(..))
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = { cells :: Array2 (Selectable Int), i :: Int, j :: Int }

newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import setCellSelection :: MatrixViewHelpers -> UIHelpers -> MatrixView -> Select -> D3.Selection -> Effect Unit
foreign import createElement :: UIHelpers -> MatrixView -> D3.Selection -> Effect D3.Selection

setBorderStyles :: IntMatrix -> D3.Selection -> Effect Unit
setBorderStyles matrix rootElement = do
   hBorders <- D3.selectAll ".matrix-cell-hBorder" rootElement
   foreachE hBorders \border -> do
      coord :: MatrixBorderCoordinate <- D3.datum border
      void $ D3.setAttrs [ "style" ↦ hBorderStyles matrix coord ] border
   vBorders <- D3.selectAll ".matrix-cell-vBorder" rootElement
   foreachE vBorders \border -> do
      coord :: MatrixBorderCoordinate <- D3.datum border
      void $ D3.setAttrs [ "style" ↦ vBorderStyles matrix coord ] border

instance Viewable MatrixView Unit where
   isLeaf = const false
   createElement _ = createElement uiHelpers
   setSelection _ mv@(MatrixView { matrix }) select rootElement = do
      setCellSelection matrixViewHelpers uiHelpers mv select rootElement
      setBorderStyles matrix rootElement

type MatrixViewHelpers =
   { eventListener :: (Event -> Effect Unit) -> Effect EventListener
   , withElement :: Select -> Event -> Effect Unit
   }

data ShadowDirection = North | South | East | West | None

matrixViewHelpers :: MatrixViewHelpers
matrixViewHelpers =
   { eventListener
   , withElement
   }
   where
   element :: ViewSelSetter MatrixCellCoordinate
   element { i, j } = matrixElement i j

   withElement sel = sel <<< uncurry element <<< selectionEventData'

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

matrixRep :: MatrixRep (SelStates 𝕊) -> IntMatrix
matrixRep (MatrixRep (vss × MatrixDim (i × _) × MatrixDim (j × _))) =
   { cells: (unpack int <$> _) <$> vss, i, j }

-- 1-based indices of selected cell; see data binding in .js
type MatrixCellCoordinate = { i :: Int, j :: Int }
type MatrixBorderCoordinate = { i :: Int, j :: Int }
