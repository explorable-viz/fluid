module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, 𝕊, from, record)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.LinkedText (LinkedText)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.MultiView (MultiView(..))
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..), arrayDictToArray2, defaultFilter, headers)
import App.View.Util (View, pack)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import DataType (cBarChart, cCons, cLineChart, cLinkedText, cMultiView, cNil, cScatterPlot)
import Val (BaseVal(..), Val(..))

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
-- Ignore view state for now..
view :: Partial => String -> Val (SelState 𝕊) -> Maybe View -> View
view title (Val _ (Constr c (u : Nil))) _
   | c == cBarChart = pack (record from u :: BarChart)
   | c == cLineChart = pack (record from u :: LineChart)
   | c == cScatterPlot = pack (record from u :: ScatterPlot)
   | c == cLinkedText = pack (from u :: LinkedText)
   | c == cMultiView = pack (MultiView (vws <*> (const Nothing <$> vws)))
        where
        vws = view title <$> from u
view title u@(Val _ (Constr c _)) _
   | c == cNil || c == cCons = pack (TableView { title, filter: defaultFilter, colNames, rows })
        where
        records = record identity <$> from u
        colNames = headers records
        rows = arrayDictToArray2 colNames records
view title (Val _ (Matrix r)) _ =
   pack (MatrixView { title, matrix: matrixRep r })
