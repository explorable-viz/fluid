module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, Selector, ViewSelector, 𝕊, eventData, from, record, selClasses, selClassesFor, selectionEventData)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart) as View
import App.View.BarChart (barChartSelector, drawBarChart')
import App.View.LineChart (LineChart) as View
import App.View.LineChart (drawLineChart', lineChartSelector)
import App.View.MatrixView (MatrixView(..)) as View
import App.View.MatrixView (drawMatrix, matrixRep, matrixViewSelector)
import App.View.ScatterPlot (ScatterPlot) as View
import App.View.ScatterPlot (drawScatterPlot, scatterPlotSelector)
import App.View.TableView (FilterToggler, drawTable', filterToggler, tableViewSelector)
import App.View.TableView (TableView(..)) as View
import App.View.Util (HTMLId, UIHelpers)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (fst, snd, uncurry)
import DataType (cBarChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Effect (Effect)
import Lattice ((∨))
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

data View
   -- one for each constructor of the Fluid 'Plot' data type
   = BarChart View.BarChart
   | LineChart View.LineChart
   | ScatterPlot View.ScatterPlot
   | MultiView (Dict View)
   -- plus default visualisations for specific kinds of value
   | MatrixView View.MatrixView
   | TableView View.TableView

type Redraw = Selector Val -> Effect Unit

newtype View2 = View2 (forall r. (forall a. View' a => a -> r) -> r)

class View' a where
   drawView' :: HTMLId -> String -> Redraw -> a -> Effect Unit

instance View' View.BarChart where
   drawView' divId suffix redraw vw =
      drawBarChart' { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw barChartSelector

instance View' View.LineChart where
   drawView' divId suffix redraw vw =
      drawLineChart' { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw lineChartSelector

instance View' View.MatrixView where
   drawView' divId suffix redraw vw =
      drawMatrix { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw matrixViewSelector

instance View' (Dict View2) where
   drawView' divId _ redraw vws =
      sequence_ $ mapWithKey (\x -> drawView2 divId x (multiPlotEntry x >>> redraw)) vws

instance View' View.ScatterPlot where
   drawView' divId suffix redraw vw =
      drawScatterPlot { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw scatterPlotSelector

instance View' View.TableView where
   drawView' divId suffix redraw vw = do
      toggleListener <- filterToggleListener filterToggler
      drawTable' toggleListener { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw tableViewSelector
      where
      filterToggleListener :: FilterToggler -> Effect EventListener
      filterToggleListener toggler = eventListener (eventData >>> toggler >>> (\_ -> identity) >>> redraw)

selListener' :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener' redraw selector = eventListener (selectionEventData >>> uncurry selector >>> redraw)

pack :: forall a. View' a => a -> View2
pack x = View2 \k -> k x

unpack :: forall r. View2 -> (forall a. View' a => a -> r) -> r
unpack (View2 vw) k = vw k

view2 :: Partial => String -> Val (SelState 𝕊) -> View2
view2 _ (Val _ (Constr c (u : Nil))) | c == cBarChart =
   pack (record from u :: View.BarChart)
view2 _ (Val _ (Constr c (u : Nil))) | c == cLineChart =
   pack (record from u :: View.LineChart)
view2 title (Val _ (Matrix r)) =
   pack (View.MatrixView { title, matrix: matrixRep r })
view2 title (Val _ (Constr c (u : Nil))) | c == cMultiPlot =
   pack (view2 title <$> from u :: Dict View2)
view2 _ (Val _ (Constr c (u : Nil))) | c == cScatterPlot =
   pack (record from u :: View.ScatterPlot)
view2 title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   pack (View.TableView { title, filter: true, table: record identity <$> from u })

drawView2 :: HTMLId -> String -> Redraw -> View2 -> Effect Unit
drawView2 divId suffix redraw vw =
   unpack vw (drawView' divId suffix redraw)

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw = case _ of
   MatrixView vw -> drawMatrix { uiHelpers, divId, suffix, view: vw } =<< selListener matrixViewSelector
   TableView vw -> do
      toggleListener <- filterToggleListener filterToggler
      drawTable' toggleListener { uiHelpers, divId, suffix, view: vw } =<< selListener tableViewSelector
   LineChart vw -> drawLineChart' { uiHelpers, divId, suffix, view: vw } =<< selListener lineChartSelector
   BarChart vw -> drawBarChart' { uiHelpers, divId, suffix, view: vw } =<< selListener barChartSelector
   ScatterPlot vw -> drawScatterPlot { uiHelpers, divId, suffix, view: vw } =<< selListener scatterPlotSelector
   MultiView vws -> sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws
   where
   selListener :: forall a. ViewSelector a -> Effect EventListener
   selListener selector = eventListener (selectionEventData >>> uncurry selector >>> redraw)

   filterToggleListener :: FilterToggler -> Effect EventListener
   filterToggleListener toggler = eventListener (eventData >>> toggler >>> (\_ -> identity) >>> redraw)

-- Convert annotated value to appropriate View, discarding top-level annotations for now.
view :: Partial => String -> Val (SelState 𝕊) -> View
view _ (Val _ (Constr c (u : Nil))) | c == cBarChart =
   BarChart (record from u)
view _ (Val _ (Constr c (u : Nil))) | c == cLineChart =
   LineChart (record from u)
view title (Val _ (Matrix r)) =
   MatrixView (View.MatrixView { title, matrix: matrixRep r })
view title (Val _ (Constr c (u : Nil))) | c == cMultiPlot =
   MultiView (view title <$> from u)
view _ (Val _ (Constr c (u : Nil))) | c == cScatterPlot =
   ScatterPlot (record from u)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   TableView (View.TableView { title, filter: true, table: record identity <$> from u })

uiHelpers :: UIHelpers
uiHelpers =
   { val: fst
   , selState: snd
   , join: (∨)
   , selClasses
   , selClassesFor
   }
