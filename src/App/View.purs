module App.View where

import Prelude hiding (absurd)

import App.Util (ReactState, ViewSelector, 𝕊, from, recordℝ, selectionEventData)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (RBarChart)
import App.View.LineChart (RLineChart)
import App.View.MatrixView (RMatrixView(..), matrixRRep)
import App.View.ScatterPlot (RScatterPlot)
import App.View.TableView (RTableView(..), TableViewState)
import App.View.Util (class Drawable, HTMLId, Redraw, draw)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (uncurry)
import DataType (cBarChart, cCons, cLineChart, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Effect (Effect)
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

data View
   -- one for each constructor of the Fluid 'Plot' data type
   = BarChart' RBarChart
   | LineChart' RLineChart
   | ScatterPlot' RScatterPlot
   | MultiView' MultiView
   -- plus default visualisations for specific kinds of value
   | MatrixView' RMatrixView
   | TableView' TableViewState RTableView

newtype MultiView = MultiView (Dict View)

selListener :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> redraw)

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
-- View state update (e.g. toggle filter) is WIP.

view :: Partial => String -> Val (ReactState 𝕊) -> Maybe View -> View
view _ (Val _ (Constr c (u : Nil))) _ | c == cBarChart =
   BarChart' (recordℝ from u)
view _ (Val _ (Constr c (u : Nil))) _ | c == cLineChart =
   --editing reflect class needed to change the record?
   LineChart' (recordℝ from u)
--LineChart' (record from (fromℝ <$> u))
view title (Val _ (Matrix r)) _ =
   {-MatrixView' (MatrixView { title, matrix: matrixRep (fromℝ <$> r) })-}
   {-matrixRRep required-}
   MatrixView' (RMatrixView { title, matrix: matrixRRep (r) })
view title (Val _ (Constr c (u : Nil))) vw | c == cMultiPlot =
   MultiView' (MultiView vws)
   where
   vws = case vw of
      Nothing -> let vws' = from u in view title <$> vws' <*> (const Nothing <$> vws')
      Just (MultiView' vws') -> view title <$> from u <*> (Just <$> unwrap vws')
view _ (Val _ (Constr c (u : Nil))) _ | c == cScatterPlot =
   ScatterPlot' (recordℝ from u)
view title u@(Val _ (Constr c _)) vw | c == cNil || c == cCons =
   TableView' vwState (RTableView { title, table: recordℝ identity <$> from u })
   where
   vwState = case vw of
      Nothing -> { filter: false }
      Just (TableView' vwState' _) -> vwState'

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw = case _ of
   BarChart' vw -> draw divId suffix redraw vw unit
   LineChart' vw -> draw divId suffix redraw vw unit
   ScatterPlot' vw -> draw divId suffix redraw vw unit
   MultiView' vw -> draw divId suffix redraw vw unit
   MatrixView' vw -> draw divId suffix redraw vw unit
   TableView' vwState vw -> draw divId suffix redraw vw vwState

instance Drawable MultiView Unit where
   draw divId _ redraw (MultiView vws) _ =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

derive instance Newtype MultiView _

{-}
view :: Partial => String -> Val (SelState 𝕊) -> Maybe View -> View
view _ (Val _ (Constr c (u : Nil))) _ | c == cBarChart =
   BarChart' (record from (fromℝ <$> (cheatToℝ <$> u)))
view _ (Val _ (Constr c (u : Nil))) _ | c == cLineChart =
   --editing reflect class eneded to change the record.
   LineChart' (record from (fromℝ <$> (cheatToℝ <$> u)))
view title (Val _ (Matrix r)) _ =
   MatrixView' (MatrixView { title, matrix: matrixRep (fromℝ <$> (cheatToℝ <$> r)) })
view title (Val _ (Constr c (u : Nil))) vw | c == cMultiPlot =
   MultiView' (MultiView vws)
   where
   vws = case vw of
      Nothing -> let vws' = from (fromℝ <$> (cheatToℝ <$> u)) in view title <$> vws' <*> (const Nothing <$> vws')
      Just (MultiView' vws') -> view title <$> from (fromℝ <$> (cheatToℝ <$> u)) <*> (Just <$> unwrap vws')
view _ (Val _ (Constr c (u : Nil))) _ | c == cScatterPlot =
   ScatterPlot' (record from (fromℝ <$> (cheatToℝ <$> u)))
view title u@(Val _ (Constr c _)) vw | c == cNil || c == cCons =
   TableView' vwState (TableView { title, table: record identity <$> from (fromℝ <$> (cheatToℝ <$> u)) })
   where
   vwState = case vw of
      Nothing -> { filter: false }
      Just (TableView' vwState' _) -> vwState'
-}