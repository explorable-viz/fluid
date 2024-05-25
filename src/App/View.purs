module App.View where

import Prelude hiding (absurd)

import App.Util (HTMLId, OnSel, SelState, 𝕊, from, record, uiHelpers)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart') as View
import App.View.BarChart (barChartHandler, barChartSelState, drawBarChart)
import App.View.BubbleChart (BubbleChart) as View
import App.View.BubbleChart (bubbleChartHandler, drawBubbleChart)
import App.View.LineChart (LineChart) as View
import App.View.LineChart (drawLineChart, lineChartHandler)
import App.View.MatrixView (MatrixView(..)) as View
import App.View.MatrixView (drawMatrix, matrixRep, matrixViewHandler)
import App.View.ScatterPlot (ScatterPlot) as View
import App.View.ScatterPlot (drawScatterPlot, scatterPlotHandler)
import App.View.TableView (TableView(..)) as View
import App.View.TableView (drawTable, tableViewHandler)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import DataType (cBarChart, cBubbleChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Effect (Effect)
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (eventListener)

data View
   -- one for each constructor of the Fluid 'Plot' data type
   = BarChart View.BarChart'
   | BubbleChart View.BubbleChart
   | LineChart View.LineChart
   | ScatterPlot View.ScatterPlot
   | MultiView (Dict View)
   -- plus default visualisations for specific kinds of value
   | MatrixView View.MatrixView
   | TableView View.TableView

drawView :: HTMLId -> String -> OnSel -> View -> Effect Unit
drawView divId suffix onSel = case _ of
   MatrixView vw -> drawMatrix { uiHelpers, divId, suffix, view: vw } =<< eventListener (onSel <<< matrixViewHandler)
   TableView vw -> drawTable { uiHelpers, divId, suffix, view: vw } =<< eventListener (onSel <<< tableViewHandler)
   LineChart vw -> drawLineChart { uiHelpers, divId, suffix, view: vw } =<< eventListener (onSel <<< lineChartHandler)
   BarChart vw -> drawBarChart { uiHelpers, divId, suffix, view: vw } =<< eventListener (onSel <<< barChartHandler)
   BubbleChart vw -> drawBubbleChart { uiHelpers, divId, suffix, view: vw } =<< eventListener (onSel <<< bubbleChartHandler)
   ScatterPlot vw -> drawScatterPlot { uiHelpers, divId, suffix, view: vw } =<< eventListener (onSel <<< scatterPlotHandler)
   MultiView vws -> sequence_ $ mapWithKey (\x -> drawView divId x (onSel <<< multiPlotEntry x)) vws

-- Convert sliced value to appropriate View, discarding top-level annotations for now.
view :: Partial => String -> Val (SelState 𝕊) -> View
view _ (Val _ (Constr c (u1 : Nil))) | c == cBarChart =
   BarChart { chart, selData: barChartSelState chart }
   where
   chart = record from u1
view _ (Val _ (Constr c (u1 : Nil))) | c == cBubbleChart =
   BubbleChart (record from u1)
view _ (Val _ (Constr c (u1 : Nil))) | c == cLineChart =
   LineChart (record from u1)
view title (Val _ (Constr c (u1 : Nil))) | c == cMultiPlot =
   MultiView (view title <$> from u1)
view _ (Val _ (Constr c (u1 : Nil))) | c == cScatterPlot =
   ScatterPlot (record from u1)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   TableView (View.TableView { title, filter: true, table: record identity <$> from u })
view title (Val _ (Matrix r)) =
   MatrixView (View.MatrixView { title, matrix: matrixRep r })
