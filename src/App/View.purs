module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, ViewSelector, 𝕊, eventData, from, record, selClasses, selClassesFor, selectionEventData)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart, barChartSelector, drawBarChart')
import App.View.LineChart (LineChart, drawLineChart', lineChartSelector)
import App.View.MatrixView (MatrixView(..), drawMatrix, matrixRep, matrixViewSelector)
import App.View.ScatterPlot (ScatterPlot, drawScatterPlot, scatterPlotSelector)
import App.View.TableView (FilterToggler, TableView(..), TableViewState(..), drawTable', filterToggler, tableViewSelector)
import App.View.Util (HTMLId, UIHelpers, Redraw)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (fst, snd, uncurry)
import DataType (cBarChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Effect (Effect)
import Lattice ((∨))
import Util (type (×), (×), spy)
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

newtype View = View (forall r. (forall a b. Drawable a b => a × b -> r) -> r)

class Drawable a b | a -> b where
   initialState :: a -> b
   draw :: HTMLId -> String -> Redraw -> a -> b -> Effect Unit

instance Drawable BarChart Unit where
   initialState _ = unit
   draw divId suffix redraw vw _ =
      drawBarChart' { uiHelpers, divId, suffix, view: vw } =<< selListener redraw barChartSelector

instance Drawable LineChart Unit where
   initialState _ = unit
   draw divId suffix redraw vw _ =
      drawLineChart' { uiHelpers, divId, suffix, view: vw } =<< selListener redraw lineChartSelector

instance Drawable MatrixView Unit where
   initialState _ = unit
   draw divId suffix redraw vw _ =
      drawMatrix { uiHelpers, divId, suffix, view: vw } =<< selListener redraw matrixViewSelector

instance Drawable (Dict View) Unit where
   initialState _ = unit
   draw divId _ redraw vws _ =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

instance Drawable ScatterPlot Unit where
   initialState _ = unit
   draw divId suffix redraw vw _ =
      drawScatterPlot { uiHelpers, divId, suffix, view: vw } =<< selListener redraw scatterPlotSelector

instance Drawable TableView TableViewState where
   initialState _ = TableViewState { filter: true }
   draw divId suffix redraw vw _ = do
      toggleListener <- filterToggleListener filterToggler
      drawTable' toggleListener { uiHelpers, divId, suffix, view: vw } =<< selListener redraw tableViewSelector
      where
      filterToggleListener :: FilterToggler -> Effect EventListener
      filterToggleListener toggler =
         eventListener (eventData >>> toggler >>> (\_ -> spy "TODO" identity) >>> redraw)

selListener :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener redraw selector = eventListener (selectionEventData >>> uncurry selector >>> redraw)

pack :: forall a b. Drawable a b => a -> View
pack x = View (_ $ (x × initialState x))

unpack :: forall r. View -> (forall a b. Drawable a b => a × b -> r) -> r
unpack (View vw) k = vw k

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
view :: Partial => String -> Val (SelState 𝕊) -> View
view _ (Val _ (Constr c (u : Nil))) | c == cBarChart =
   pack (record from u :: BarChart)
view _ (Val _ (Constr c (u : Nil))) | c == cLineChart =
   pack (record from u :: LineChart)
view title (Val _ (Matrix r)) =
   pack (MatrixView { title, matrix: matrixRep r })
view title (Val _ (Constr c (u : Nil))) | c == cMultiPlot =
   pack (view title <$> from u :: Dict View)
view _ (Val _ (Constr c (u : Nil))) | c == cScatterPlot =
   pack (record from u :: ScatterPlot)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   pack (TableView { title, filter: true, table: record identity <$> from u })

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw vw = unpack vw (uncurry $ draw divId suffix redraw)

uiHelpers :: UIHelpers
uiHelpers =
   { val: fst
   , selState: snd
   , join: (∨)
   , selClasses
   , selClassesFor
   }
