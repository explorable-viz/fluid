module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, 𝕊, from, record)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.LinkedText (LinkedText)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..), TableViewState)
import App.View.Util (class Drawable, HTMLId, Redraw, draw)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import DataType (cBarChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot, cText)
import Dict (Dict)
import Effect (Effect)
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))

data View
   -- one for each constructor of the Fluid 'Plot' data type
   = BarChart' BarChart
   | LineChart' LineChart
   | ScatterPlot' ScatterPlot
   | MultiView' MultiView
   -- plus default visualisations for specific kinds of value
   | MatrixView' MatrixView
   | TableView' TableViewState TableView
   | LinkedText' LinkedText

newtype MultiView = MultiView (Dict View)

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
-- View state update (e.g. toggle filter) is WIP.
view :: Partial => String -> Val (SelState 𝕊) -> Maybe View -> View
view _ (Val _ (Constr c (u : Nil))) _ | c == cBarChart =
   BarChart' (record from u)
view _ (Val _ (Constr c (u : Nil))) _ | c == cText =
   LinkedText' (from u)
view _ (Val _ (Constr c (u : Nil))) _ | c == cLineChart =
   LineChart' (record from u)
view title (Val _ (Matrix r)) _ =
   MatrixView' (MatrixView { title, matrix: matrixRep r })
view title (Val _ (Constr c (u : Nil))) vw | c == cMultiPlot =
   MultiView' (MultiView vws)
   where
   vws = case vw of
      Nothing -> let vws' = from u in view title <$> vws' <*> (const Nothing <$> vws')
      Just (MultiView' vws') -> view title <$> from u <*> (Just <$> unwrap vws')
view _ (Val _ (Constr c (u : Nil))) _ | c == cScatterPlot =
   ScatterPlot' (record from u)
view title u@(Val _ (Constr c _)) vw | c == cNil || c == cCons =
   TableView' vwState (TableView { title, table: record identity <$> from u })
   where
   vwState = case vw of
      Nothing -> { filter: true }
      Just (TableView' vwState' _) -> vwState'

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw = case _ of
   BarChart' vw -> draw redraw divId suffix vw unit
   LineChart' vw -> draw redraw divId suffix vw unit
   ScatterPlot' vw -> draw redraw divId suffix vw unit
   MultiView' vw -> draw redraw divId suffix vw unit
   MatrixView' vw -> draw redraw divId suffix vw unit
   TableView' vwState vw -> draw redraw divId suffix vw vwState
   LinkedText' vw -> draw redraw divId suffix vw unit

instance Drawable MultiView Unit where
   draw redraw divId _ (MultiView vws) _ =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

derive instance Newtype MultiView _
