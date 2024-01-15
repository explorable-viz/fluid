module App.View where

import Prelude hiding (absurd)

import App.BarChart (BarChart, barChartHandler, drawBarChart)
import App.BubbleChart (BubbleChart, bubbleChartHandler, drawBubbleChart)
import App.LineChart (LineChart, drawLineChart, lineChartHandler)
import App.MatrixView (MatrixView(..), drawMatrix, matrixRep, matrixViewHandler)
import App.ScatterPlot (ScatterPlot, drawScatterPlot, scatterPlotHandler)
import App.TableView (TableView(..), drawTable, tableViewHandler)
import App.Util (HTMLId, OnSel, Sel, from, record)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import DataType (cBarChart, cBubbleChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict, mapWithKey)
import Effect (Effect)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (eventListener)

data View
   -- one for each constructor of the Fluid 'Plot' data type
   = BarChartFig BarChart
   | BubbleChartFig BubbleChart
   | LineChartFig LineChart
   | ScatterPlotFig ScatterPlot
   | MultiView (Dict View)
   -- plus default visualisations for specific kinds of value
   | MatrixFig MatrixView
   | TableFig TableView

drawView :: HTMLId -> String -> OnSel -> View -> Effect Unit
drawView divId suffix onSel = case _ of
   MatrixFig vw -> drawMatrix divId suffix vw =<< eventListener (onSel <<< matrixViewHandler)
   TableFig vw -> drawTable divId suffix vw =<< eventListener (onSel <<< tableViewHandler)
   LineChartFig vw -> drawLineChart divId suffix vw =<< eventListener (onSel <<< lineChartHandler)
   BarChartFig vw -> drawBarChart divId suffix vw =<< eventListener (onSel <<< barChartHandler)
   BubbleChartFig vw -> drawBubbleChart divId suffix vw =<< eventListener (onSel <<< bubbleChartHandler)
   ScatterPlotFig vw -> drawScatterPlot divId suffix vw =<< eventListener (onSel <<< scatterPlotHandler)
   MultiView vws -> sequence_ $ mapWithKey (flip (drawView divId) onSel) vws

-- Convert sliced value to appropriate View, discarding top-level annotations for now.
view :: Partial => String -> Val Sel -> View
view _ (Val _ (Constr c (u1 : Nil))) | c == cBarChart =
   BarChartFig (record from u1)
view _ (Val _ (Constr c (u1 : Nil))) | c == cBubbleChart =
   BubbleChartFig (record from u1)
view _ (Val _ (Constr c (u1 : Nil))) | c == cLineChart =
   LineChartFig (record from u1)
view title (Val _ (Constr c (u1 : Nil))) | c == cMultiPlot =
   MultiView (view title <$> from u1)
view _ (Val _ (Constr c (u1 : Nil))) | c == cScatterPlot =
   ScatterPlotFig (record from u1)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   TableFig (TableView { title, filter: true, table: record identity <$> from u })
view title (Val _ (Matrix r)) =
   MatrixFig (MatrixView { title, matrix: matrixRep r })
