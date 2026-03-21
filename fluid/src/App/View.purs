module App.View where

import Prelude hiding (absurd)

import App.Util (Dimensions(..), SelStates, Selectable, 𝕊, dict, get_intOrNumber)
import App.View.BarChart (BarChart(..))
import App.View.DocView (DocView(..))
import App.View.LineChart (LineChart(..), LinePlot(..))
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.MultiView (MultiView(..))
import App.View.Paragraph (Paragraph(..))
import App.View.ScatterPlot (ScatterPlot(..))
import App.View.Segment (Segment(..))
import App.View.StackedBar (StackedBar(..))
import App.View.TableView (TableView(..), arrayDictToArray2, headers)
import App.View.Text (Text(..))
import App.View.Util (Filter(..), View, Options, pack)
import App.View.Util.Axes (Orientation, orientation)
import App.View.Util.Point (Point(..))
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd)
import DataType (cBarChart, cCons, cLineChart, cLinePlot, cLink, cMultiView, cNil, cParagraph, cScatterPlot, cText, f_caption, f_height, f_labels, f_legend, f_name, f_plots, f_points, f_segments, f_size, f_stackedBars, f_tickLabels, f_width, f_x, f_y, f_z)
import Dict (Dict)
import Link (Link(..))
import Primitive (boolean, int, string, typeError)
import Primitive (unpack) as P
import Util (type (×), error, (×))
import Util.Map (get, mapWithKey)
import Val (BaseVal(..), DictRep(..), Val(..))

-- TODO: merge with 'view' below.
view' :: Partial => Options -> String -> Val (SelStates 𝕊) -> View
view' options title v@(Val _ v_opt _) =
   pack $ DocView { doc: viewParagraph <$> v_opt, view: view options title v }
   where
   viewParagraph (Val _ _ (Constr c (u : Nil))) | c == cParagraph =
      Paragraph (view options "" <$> from u)

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
-- TODO: given the typeError clause, Partial no longer needed
view :: Partial => Options -> String -> Val (SelStates 𝕊) -> View
view options title v@(Val α _ u') = case u' of
   Int n -> pack (Text (show n × α))
   Float n -> pack (Text (show n × α))
   Str str -> pack (Text (str × α))
   Constr c (u : Nil)
      | c == cText -> pack (from u :: Text)
      | c == cBarChart -> pack (dict from u :: BarChart)
      | c == cLineChart -> pack (dict from u :: LineChart)
      | c == cScatterPlot -> pack (dict from u :: ScatterPlot)
      | c == cMultiView -> pack (MultiView (view options "" <$> from u))
      | c == cParagraph -> pack (Paragraph (view options "" <$> from u))
   Constr c (_ : _ : Nil)
      -- more consistent with other views for Link to take single argument of record type
      | c == cLink -> pack (from v :: Link)
   Constr c _
      | c == cNil || c == cCons ->
           if tableView then
              let
                 rowFilter = fromMaybe Interactive options.rowFilter
                 records = dict identity <$> vs
                 colNames = headers records
                 rows = arrayDictToArray2 colNames records <#> map snd
              in
                 pack (TableView { title, rowFilter, colNames, rows })
           else pack (MultiView $ view options "" <$> vs)
           where
           tableView = case A.uncons vs of
              Just { head: Val _ _ (Dictionary _) } -> true
              Just { head: Val _ _ _ } -> false
              Nothing -> true
           vs = from v :: Array (Val (SelStates 𝕊))
   Matrix r ->
      pack (MatrixView { title, matrix: matrixRep r })
   Dictionary (DictRep d) ->
      pack (viewDict d)
   _ -> typeError u' "Viewable"
   where
   viewDict :: Partial => Dict (SelStates 𝕊 × Val (SelStates 𝕊)) -> Dict (View × View)
   viewDict = mapWithKey \k (α' × v') -> pack (Text (k × α')) × view options k v'

class Reflect a b where
   from :: Partial => a -> b

instance Reflect (Val a) (Dict (a × Val a)) where
   from (Val _ _ (Dictionary (DictRep d))) = d

instance Reflect (Val a) (Array (Val a)) where
   from (Val _ _ (Constr c Nil)) | c == cNil = []
   from (Val _ _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

instance Reflect (Val a) (NonEmptyArray (Val a)) where
   from (Val _ _ (Constr c Nil)) | c == cNil = error "expected non-empty list"
   from (Val _ _ (Constr c (u1 : u2 : Nil))) | c == cCons = cons' u1 (from u2)

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) (Dimensions (Selectable Int)) where
   from r = Dimensions
      { width: P.unpack int (snd (get f_width r))
      , height: P.unpack int (snd (get f_height r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) BarChart where
   from r = BarChart
      { caption: P.unpack string (snd (get f_caption r))
      , stackedBars: dict from <$> from (snd (get f_stackedBars r))
      , size: dict from (snd (get f_size r))
      , tickLabels: dict from (snd (get f_tickLabels r))
      , legend: P.unpack boolean (snd (get f_legend r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) StackedBar where
   from r = StackedBar
      { x: P.unpack string (snd (get f_x r))
      , segments: dict from <$> from (snd (get f_segments r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) Segment where
   from :: Partial => Dict (SelStates 𝕊 × Val (SelStates 𝕊)) -> Segment
   from r = Segment
      { y: P.unpack string (snd (get f_y r))
      , z: get_intOrNumber f_z r
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) LinePlot where
   from r = LinePlot
      { name: P.unpack string (snd (get f_name r))
      , points: dict from <$> from (snd (get f_points r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) LineChart where
   from r = LineChart
      { size: dict from (snd (get f_size r))
      , tickLabels: dict from (snd (get f_tickLabels r))
      , caption: P.unpack string (snd (get f_caption r))
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
      { x: P.unpack string (snd (get f_x r))
      , y: P.unpack string (snd (get f_y r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) (Point Orientation) where
   from r = Point
      { x: P.unpack orientation (snd (get f_x r))
      , y: P.unpack orientation (snd (get f_y r))
      }

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) ScatterPlot where
   from r = ScatterPlot
      { caption: P.unpack string (snd (get f_caption r))
      , points: dict from <$> from (snd (get f_points r))
      , labels: dict from (snd (get f_labels r))
      }

instance Reflect (Val (SelStates 𝕊)) Text where
   from (Val α _ v) = case v of
      Str s -> Text (s × α)
      _ -> typeError v "Text expects string"

instance Reflect (Val (SelStates 𝕊)) Link where
   from (Val _ _ u) = case u of
      (Constr c (v : (Val α' _ (Str s) : Nil))) | c == cLink -> Link v (s × α')
      _ -> typeError u "Link expects string as second argument"
