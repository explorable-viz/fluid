module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, ViewSelector, 𝕊, from, record, selectionEventData)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..))
import App.View.Util (class Drawable, HTMLId, Redraw, draw, initialState)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (uncurry)
import DataType (cBarChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Effect (Effect)
import Util (type (×), (×))
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

newtype View = View (forall r. (forall a b. Drawable a b => a × b -> r) -> r)

selListener :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> redraw)

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
   pack (MultiView (view title <$> from u))
view _ (Val _ (Constr c (u : Nil))) | c == cScatterPlot =
   pack (record from u :: ScatterPlot)
view title u@(Val _ (Constr c _)) | c == cNil || c == cCons =
   pack (TableView { title, table: record identity <$> from u })

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw vw = unpack vw (uncurry $ draw divId suffix redraw)

-- Newtype avoids orphan instance/cyclic dependency
newtype MultiView = MultiView (Dict View)

instance Drawable MultiView Unit where
   initialState _ = unit

   draw divId _ redraw (MultiView vws) _ =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws
