module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, ViewSelector, 𝕊, from, record, selectionEventData)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..))
import App.View.Util (class Drawable, HTMLId, Redraw, draw)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (uncurry)
import DataType (cBarChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Effect (Effect)
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

data View
   -- one for each constructor of the Fluid 'Plot' data type
   = BarChart BarChart
   | LineChart LineChart
   | ScatterPlot ScatterPlot
   | MultiView' MultiView
   -- plus default visualisations for specific kinds of value
   | MatrixView' MatrixView
   | TableView' TableView

selListener :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> redraw)

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
view :: Partial => String -> Val (SelState 𝕊) -> View
view _ (Val _ (Constr c (u : Nil))) | c == cBarChart =
   BarChart (record from u)
view _ (Val _ (Constr c (u : Nil))) | c == cLineChart =
   LineChart (record from u)
view title (Val _ (Matrix r)) =
   MatrixView' (MatrixView { title, matrix: matrixRep r })
view title (Val _ (Constr c (u : Nil))) | c == cMultiPlot =
   MultiView' (MultiView (view title <$> from u))
view _ (Val _ (Constr c (u : Nil))) | c == cScatterPlot =
   ScatterPlot (record from u)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   TableView' (TableView { title, table: record identity <$> from u })

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw = case _ of
   BarChart x -> draw divId suffix redraw x unit
   LineChart x -> draw divId suffix redraw x unit
   ScatterPlot x -> draw divId suffix redraw x unit
   MultiView' x -> draw divId suffix redraw x unit
   MatrixView' x -> draw divId suffix redraw x unit
   TableView' x -> draw divId suffix redraw x { filter: true }

-- Newtype avoids orphan instance/cyclic dependency
newtype MultiView = MultiView (Dict View)

instance Drawable MultiView Unit where
   initialState _ = unit

   draw divId _ redraw (MultiView vws) _ =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws
