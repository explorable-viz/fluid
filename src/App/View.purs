module App.View where

import Prelude hiding (absurd)

import App.Util (Dimensions(..), SelStates, Selectable, 𝕊, dict, get_intOrNumber, inert)
import App.View.BarChart (BarChart(..))
import App.View.DocView (DocView(..))
import App.View.LineChart (LineChart(..), LinePlot(..))
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.MultiView (MultiView(..))
import App.View.Paragraph (Paragraph(..))
import App.View.ScatterPlot (ScatterPlot(..))
import App.View.Segment (Segment(..))
import App.View.StackedBar (StackedBar(..))
import App.View.TableView (TableView(..), arrayDictToArray2, defaultFilter, headers)
import App.View.Text (Text(..))
import App.View.Util (View', pack)
import App.View.Util.Axes (Orientation, orientation)
import App.View.Util.Point (Point(..))
import Data.Array ((:)) as A
import Data.Array (fromFoldable)
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import DataType (cBarChart, cCons, cLineChart, cLinePlot, cLink, cMultiView, cNil, cParagraph, cScatterPlot, cText, f_caption, f_labels, f_name, f_plots, f_points, f_segments, f_size, f_stackedBars, f_tickLabels, f_x, f_y, f_z)
import Dict (Dict)
import Doc (DocCommentElem(..))
import Link (Link(..))
import Primitive (int, string, typeError, unpack)
import Util (type (×), error, (×))
import Util.Map (get)
import Val (BaseVal(..), DictRep(..), Val(..), ValDoc(..))

view' :: Partial => String -> Val (SelStates 𝕊) -> Maybe View' -> View'
view' title v@(Val _ doc _) _ =
   pack $ DocView { doc: viewPara doc, view: view title v Nothing }

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
-- Ignore view state for now..
view :: Partial => String -> Val (SelStates 𝕊) -> Maybe View' -> View'
view title (Val _ _ (Constr c (u : Nil))) _
   | c == cText = pack (from u :: Text)
   | c == cBarChart = pack (dict from u :: BarChart)
   | c == cLineChart = pack (dict from u :: LineChart)
   | c == cScatterPlot = pack (dict from u :: ScatterPlot)
   | c == cMultiView = pack (MultiView (vws <*> (const Nothing <$> vws)))
        where
        vws = view title <$> ((from u :: Dict (SelStates 𝕊 × Val (SelStates 𝕊))) # map snd)
   | c == cParagraph = pack (Paragraph false (vws <*> (const Nothing <$> vws)))
        where
        vws = view title <$> from u
view _ v@(Val _ _ (Constr c (_ : _ : Nil))) _
   | c == cLink = pack (from v :: Link)
view title u@(Val _ _ (Constr c _)) _
   | c == cNil || c == cCons = pack (TableView { title, filter: defaultFilter, colNames, rows })
        where
        records = dict identity <$> from u
        colNames = headers records
        rows = arrayDictToArray2 colNames records <#> map snd
view title (Val _ _ (Matrix r)) _ =
   pack (MatrixView { title, matrix: matrixRep r })

viewPara :: Partial => ValDoc (SelStates 𝕊) -> Maybe Paragraph
viewPara None' = Nothing
viewPara (ValDoc _ doc) = Just $ Paragraph true $ fromFoldable $ formatPara $ doc
   where
   formatPara :: List (DocCommentElem Val (SelStates 𝕊)) -> List View'
   formatPara Nil = Nil
   formatPara (Token str : Token str' : xs) = formatPara $ (Token (str <> " " <> str')) : xs
   formatPara (Token str : xs) = pack (Text (str × inert)) : formatPara xs
   formatPara (Unquote (Val α _ (Int n)) : xs) = pack (Text (show n × α)) : formatPara xs
   formatPara (Unquote v : xs) = view "" v Nothing : formatPara xs

-- ======================
-- boilerplate
-- ======================

class Reflect a b where
   from :: Partial => a -> b

instance Reflect (Val (SelStates 𝕊)) (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) where
   from (Val _ _ (Dictionary (DictRep d))) = d

instance Reflect (Val (SelStates 𝕊)) (Array (Val (SelStates 𝕊))) where
   from (Val _ _ (Constr c Nil)) | c == cNil = []
   from (Val _ _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

instance Reflect (Val (SelStates 𝕊)) (NonEmptyArray (Val (SelStates 𝕊))) where
   from (Val _ _ (Constr c Nil)) | c == cNil = error "expected non-empty list"
   from (Val _ _ (Constr c (u1 : u2 : Nil))) | c == cCons = cons' u1 (from u2)

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) (Dimensions (Selectable Int)) where
   from r = Dimensions
      { width: unpack int (snd (get "width" r))
      , height: unpack int (snd (get "height" r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) BarChart where
   from r = BarChart
      { caption: unpack string (snd (get f_caption r))
      , stackedBars: dict from <$> from (snd (get f_stackedBars r))
      , size: dict from (snd (get f_size r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) StackedBar where
   from r = StackedBar
      { x: unpack string (snd (get f_x r))
      , segments: dict from <$> from (snd (get f_segments r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) Segment where
   from :: Partial => Dict (SelStates 𝕊 × Val (SelStates 𝕊)) -> Segment
   from r = Segment
      { y: unpack string (snd (get f_y r))
      , z: get_intOrNumber f_z r
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) LinePlot where
   from r = LinePlot
      { name: unpack string (snd (get f_name r))
      , points: dict from <$> from (snd (get f_points r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) LineChart where
   from r = LineChart
      { size: dict from (snd (get f_size r))
      , tickLabels: dict from (snd (get f_tickLabels r))
      , caption: unpack string (snd (get f_caption r))
      , plots: from <$> (from (snd (get f_plots r)) :: Array (Val (SelStates 𝕊))) :: Array LinePlot
      }

instance Reflect (Val (SelStates 𝕊)) LinePlot where
   from (Val _ _ (Constr c (u : Nil))) | c == cLinePlot = dict from u

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) (Point Number) where
   from r = Point
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) (Point String) where
   from r = Point
      { x: unpack string (snd (get f_x r))
      , y: unpack string (snd (get f_y r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) (Point Orientation) where
   from r = Point
      { x: unpack orientation (snd (get f_x r))
      , y: unpack orientation (snd (get f_y r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) ScatterPlot where
   from r = ScatterPlot
      { caption: unpack string (snd (get f_caption r))
      , points: dict from <$> from (snd (get f_points r))
      , labels: dict from (snd (get f_labels r))
      }

instance Reflect (Val (SelStates 𝕊)) Text where
   from (Val α _ v) = case v of
      Str s -> Text (s × α)
      _ -> typeError v "Text"

instance Reflect (Val (SelStates 𝕊)) Link where
   from (Val _ _ u) = case u of
      (Constr c (Val α doc v : (Val α' _ (Str s) : Nil))) | c == cLink -> Link (Val α doc v) (s × α')
      _ -> typeError u "Link"
