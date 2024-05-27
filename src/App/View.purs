module App.View where

import Prelude hiding (absurd)

import App.Util (HTMLId, SelState, ViewSelector, 𝕊, Selector, eventData, from, record, uiHelpers)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart) as View
import App.View.BarChart (barChartSelector, drawBarChart)
import App.View.BubbleChart (BubbleChart) as View
import App.View.BubbleChart (bubbleChartSelector, drawBubbleChart)
import App.View.LineChart (LineChart) as View
import App.View.LineChart (drawLineChart, lineChartSelector)
import App.View.MatrixView (MatrixView(..)) as View
import App.View.MatrixView (drawMatrix, matrixRep, matrixViewSelector)
import App.View.ScatterPlot (ScatterPlot) as View
import App.View.ScatterPlot (drawScatterPlot, scatterPlotSelector)
import App.View.TableView (TableView(..)) as View
import App.View.TableView (drawTable, tableViewSelector)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (uncurry)
import DataType (cBarChart, cBubbleChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Effect (Effect)
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

data View
   -- one for each constructor of the Fluid 'Plot' data type
   = BarChart View.BarChart
   | BubbleChart View.BubbleChart
   | LineChart View.LineChart
   | ScatterPlot View.ScatterPlot
   | MultiView (Dict View)
   -- plus default visualisations for specific kinds of value
   | MatrixView View.MatrixView
   | TableView View.TableView

drawView :: HTMLId -> String -> (Selector Val -> Effect Unit) -> View -> Effect Unit
drawView divId suffix redraw = case _ of
   MatrixView vw -> drawMatrix { uiHelpers, divId, suffix, view: vw } =<< listener matrixViewSelector
   TableView vw -> drawTable { uiHelpers, divId, suffix, view: vw } =<< listener tableViewSelector
   LineChart vw -> drawLineChart { uiHelpers, divId, suffix, view: vw } =<< listener lineChartSelector
   BarChart vw -> drawBarChart { uiHelpers, divId, suffix, view: vw } =<< listener barChartSelector
   BubbleChart vw -> drawBubbleChart { uiHelpers, divId, suffix, view: vw } =<< listener bubbleChartSelector
   ScatterPlot vw -> drawScatterPlot { uiHelpers, divId, suffix, view: vw } =<< listener scatterPlotSelector
   MultiView vws -> sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws
   where
   listener :: forall a. ViewSelector a -> Effect EventListener
   listener selector = eventListener (eventData >>> uncurry selector >>> redraw)

-- Convert annotated value to appropriate View, discarding top-level annotations for now.
view :: Partial => String -> Val (SelState 𝕊) -> View
view _ (Val _ (Constr c (u : Nil))) | c == cBarChart =
   BarChart (record from u)
view _ (Val _ (Constr c (u : Nil))) | c == cBubbleChart =
   BubbleChart (record from u)
view _ (Val _ (Constr c (u : Nil))) | c == cLineChart =
   LineChart (record from u)
view title (Val _ (Constr c (u : Nil))) | c == cMultiPlot =
   MultiView (view title <$> from u)
view _ (Val _ (Constr c (u : Nil))) | c == cScatterPlot =
   ScatterPlot (record from u)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   TableView (View.TableView { title, filter: true, table: record identity <$> from u })
view title (Val _ (Matrix r)) =
   MatrixView (View.MatrixView { title, matrix: matrixRep r })
