module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, 𝕊, from, record)
import App.Util.Selector (multiPlotEntry)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.LinkedText (LinkedText)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..))
import App.View.Util (class Drawable, class View', View, drawView, pack)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import DataType (cBarChart, cCons, cLineChart, cLinkedText, cMultiPlot, cNil, cScatterPlot)
import Dict (Dict)
import Util.Map (mapWithKey)
import Val (BaseVal(..), Val(..))

newtype MultiView = MultiView (Dict View)

instance View' MultiView where
   drawView' divId _ redraw (MultiView vws) =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
view :: Partial => String -> Val (SelState 𝕊) -> Maybe View -> View
view title (Val _ (Constr c (u : Nil))) _
   | c == cBarChart = pack (record from u :: BarChart)
   | c == cLineChart = pack (record from u :: LineChart)
   | c == cScatterPlot = pack (record from u :: ScatterPlot)
   | c == cLinkedText = pack (from u :: LinkedText)
   | c == cMultiPlot = pack (MultiView (vws <*> (const Nothing <$> vws)))
        where
        vws = view title <$> from u
view title u@(Val _ (Constr c _)) _
   | c == cNil || c == cCons =
        pack (TableView { title, table: record identity <$> from u }) -- ignore view state for now
view title (Val _ (Matrix r)) _ =
   pack (MatrixView { title, matrix: matrixRep r })

instance Drawable MultiView Unit where
   draw redraw { divId, view: MultiView vws } =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

derive instance Newtype MultiView _
