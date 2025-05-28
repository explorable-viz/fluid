module App.View where

import Prelude hiding (absurd)

import App.Util (SelStates, 𝕊, dict, from, inert)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.MultiView (MultiView(..))
import App.View.Paragraph (Paragraph(..), ParaFragment(..))
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..), arrayDictToArray2, defaultFilter, headers)
import App.View.Util (View, pack)
import Data.Array (fromFoldable)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import DataType (cBarChart, cCons, cLineChart, cMultiView, cNil, cParagraph, cScatterPlot)
import Dict (Dict)
import Dict as Dict
import Doc (DocCommentElem(..), DocOpt(..))
import Util (type (×), (×))
import Val (BaseVal(..), Val(..))

view' :: Partial => String -> Val (SelStates 𝕊) -> Maybe View -> View
view' title v@(Val _ doc _) _ =
   if doc == None then realView
   else
      let
         docView = viewDocComment doc
         vws = Dict.fromFoldable [ title × realView, title × docView ] :: Dict View
      in
         pack $ MultiView vws
   where
   realView = view title v Nothing

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
-- Ignore view state for now..
view :: Partial => String -> Val (SelStates 𝕊) -> Maybe View -> View
view title (Val _ _ (Constr c (u : Nil))) _
   | c == cBarChart = pack (dict from u :: BarChart)
   | c == cLineChart = pack (dict from u :: LineChart)
   | c == cScatterPlot = pack (dict from u :: ScatterPlot)
   | c == cParagraph = pack (from u :: Paragraph)
   | c == cMultiView = pack (MultiView (vws <*> (const Nothing <$> vws)))
        where
        vws = view title <$> ((from u :: Dict (SelStates 𝕊 × Val (SelStates 𝕊))) # map snd)
view title u@(Val _ _ (Constr c _)) _
   | c == cNil || c == cCons = pack (TableView { title, filter: defaultFilter, colNames, rows })
        where
        records = dict identity <$> from u
        colNames = headers records
        rows = arrayDictToArray2 colNames records <#> map snd
view title (Val _ _ (Matrix r)) _ =
   pack (MatrixView { title, matrix: matrixRep r })

viewDocComment :: Partial => DocOpt Val (SelStates 𝕊) -> View
viewDocComment (Doc doc) = pack $ Paragraph $ fromFoldable $ map viewDocElem doc
   where
   viewDocElem :: Partial => DocCommentElem Val (SelStates 𝕊) -> ParaFragment
   viewDocElem (Token str) = Text (str × inert)
   viewDocElem (Unquote val) = Viewable $ view "" val Nothing
