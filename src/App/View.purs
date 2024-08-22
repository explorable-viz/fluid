module App.View where

import Prelude hiding (absurd)

import App.Util (SelState, 𝕊, from, record)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.LinkedText (LinkedText)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.MultiView (MultiView(..))
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..), table)
import App.View.Util (View, View2(..), pack, pack2, unsafeUnpack)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import DataType (cBarChart, cCons, cLineChart, cLinkedText, cMultiView, cNil, cScatterPlot)
import Dict (Dict)
import Val (BaseVal(..), Val(..))

-- Convert annotated value to appropriate view.
-- Note: discards top-level annotation (see https://github.com/explorable-viz/fluid/issues/1033)
view :: Partial => String -> Val (SelState 𝕊) -> Maybe View -> View
view title (Val _ (Constr c (u : Nil))) _
   | c == cBarChart = pack (record from u :: BarChart)
   | c == cLineChart = pack (record from u :: LineChart)
   | c == cScatterPlot = pack (record from u :: ScatterPlot)
   | c == cLinkedText = pack (from u :: LinkedText)
   | c == cMultiView = pack (MultiView (vws <*> (const Nothing <$> vws)))
        where
        vws = view title <$> from u
view title u@(Val _ (Constr c _)) vw
   | c == cNil || c == cCons =
        pack (maybe (TableView { title, filter: true, table: table' }) (table (const table')) vw')
        where
        vw' :: Maybe TableView
        vw' = unsafeUnpack <$> vw

        table' :: Array (Dict (Val (SelState 𝕊)))
        table' = record identity <$> from u
view title (Val _ (Matrix r)) _ =
   pack (MatrixView { title, matrix: matrixRep r })

view2 :: Partial => String -> Val (SelState 𝕊) -> View2
view2 title (Val _ (Constr c (u : Nil)))
   | c == cBarChart = pack2 (record from u :: BarChart)
   | c == cLineChart = pack2 (record from u :: LineChart)
   | c == cScatterPlot = pack2 (record from u :: ScatterPlot)
   | c == cLinkedText = pack2 (from u :: LinkedText)
   | c == cMultiView = pack2 (MultiView (vws <*> (const Nothing <$> vws)))
        where
        vws = view title <$> from u
view2 title u@(Val _ (Constr c _))
   | c == cNil || c == cCons =
        pack2 (TableView { title, filter: true, table: record identity <$> from u })
view2 title (Val _ (Matrix r)) =
   pack2 (MatrixView { title, matrix: matrixRep r })
