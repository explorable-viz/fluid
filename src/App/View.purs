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

type Redraw = Selector Val -> Effect Unit
newtype View = View (forall r. (forall a. Drawable a => a -> r) -> r)

class Drawable a where
   draw :: HTMLId -> String -> Redraw -> a -> Effect Unit

instance Drawable View.BarChart where
   draw divId suffix redraw vw =
      drawBarChart' { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw barChartSelector

instance Drawable View.LineChart where
   draw divId suffix redraw vw =
      drawLineChart' { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw lineChartSelector

instance Drawable View.MatrixView where
   draw divId suffix redraw vw =
      drawMatrix { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw matrixViewSelector

instance Drawable (Dict View) where
   draw divId _ redraw vws =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

instance Drawable View.ScatterPlot where
   draw divId suffix redraw vw =
      drawScatterPlot { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw scatterPlotSelector

instance Drawable View.TableView where
   draw divId suffix redraw vw = do
      toggleListener <- filterToggleListener filterToggler
      drawTable' toggleListener { uiHelpers, divId, suffix, view: vw } =<< selListener' redraw tableViewSelector
      where
      filterToggleListener :: FilterToggler -> Effect EventListener
      filterToggleListener toggler = eventListener (eventData >>> toggler >>> (\_ -> identity) >>> redraw)

selListener' :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener' redraw selector = eventListener (selectionEventData >>> uncurry selector >>> redraw)

pack :: forall a. Drawable a => a -> View
pack x = View \k -> k x

unpack :: forall r. View -> (forall a. Drawable a => a -> r) -> r
unpack (View vw) k = vw k

-- Convert annotated value to appropriate View, discarding top-level annotations for now.
view :: Partial => String -> Val (SelState 𝕊) -> View
view _ (Val _ (Constr c (u : Nil))) | c == cBarChart =
   pack (record from u :: View.BarChart)
view _ (Val _ (Constr c (u : Nil))) | c == cLineChart =
   pack (record from u :: View.LineChart)
view title (Val _ (Matrix r)) =
   pack (View.MatrixView { title, matrix: matrixRep r })
view title (Val _ (Constr c (u : Nil))) | c == cMultiPlot =
   pack (view title <$> from u :: Dict View)
view _ (Val _ (Constr c (u : Nil))) | c == cScatterPlot =
   pack (record from u :: View.ScatterPlot)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   pack (View.TableView { title, filter: true, table: record identity <$> from u })

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw vw = unpack vw (draw divId suffix redraw)

uiHelpers :: UIHelpers
uiHelpers =
   { val: fst
   , selState: snd
   , join: (∨)
   , selClasses
   , selClassesFor
   }
