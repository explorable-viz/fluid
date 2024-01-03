module App.View where

import Prelude hiding (absurd)

import App.BarChart (BarChart, barChartHandler, drawBarChart)
import App.BubbleChart (BubbleChart, bubbleChartHandler, drawBubbleChart)
import App.LineChart (LineChart, drawLineChart, lineChartHandler)
import App.MatrixView (MatrixView(..), drawMatrix, matrixRep, matrixViewHandler)
import App.TableView (TableView(..), drawTable, tableViewHandler)
import App.Util (HTMLId, OnSel, Sel, from, record, to𝔹)
import Data.List (List(..), (:))
import DataType (cBarChart, cBubbleChart, cCons, cLineChart, cNil)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Util (absurd, error)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (eventListener)

data View
   = MatrixFig MatrixView
   | TableFig TableView
   | LineChartFig LineChart
   | BarChartFig BarChart
   | BubbleChartFig BubbleChart

drawView :: HTMLId -> Int -> OnSel -> View -> Effect Unit
drawView divId n onSel = case _ of
   MatrixFig vw -> drawMatrix divId n vw =<< eventListener (onSel <<< matrixViewHandler)
   TableFig vw -> drawTable divId n vw =<< eventListener (onSel <<< tableViewHandler)
   LineChartFig vw -> drawLineChart divId n vw =<< eventListener (onSel <<< lineChartHandler)
   BarChartFig vw -> drawBarChart divId n vw =<< eventListener (onSel <<< barChartHandler)
   BubbleChartFig vw -> drawBubbleChart divId n vw =<< eventListener (onSel <<< bubbleChartHandler)

-- Convert sliced value to appropriate View, discarding top-level annotations for now.
-- 'from' is partial; encapsulate that here.
view :: String -> Val Sel -> View
view _ (Val _ (Constr c (u1 : Nil))) | c == cBarChart =
   BarChartFig (unsafePartial $ record from $ to𝔹 <$> u1)
view _ (Val _ (Constr c (u1 : Nil))) | c == cLineChart =
   LineChartFig (unsafePartial $ record from $ to𝔹 <$> u1)
view _ (Val _ (Constr c (u1 : Nil))) | c == cBubbleChart =
   BubbleChartFig (unsafePartial $ record from $ to𝔹 <$> u1)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   TableFig (TableView { title, filter: true, table: unsafePartial $ record identity <$> from (to𝔹 <$> u) })
view title (Val _ (Matrix r)) =
   MatrixFig (MatrixView { title, matrix: matrixRep r })
view _ _ = error absurd
