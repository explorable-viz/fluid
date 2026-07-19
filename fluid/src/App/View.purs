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
import App.Util.Selector (SelSetter, constrArg, listElement)
import App.View.Util.Axes (Orientation, orientation)
import App.View.Util.Point (Point(..))
import Bind (Name)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.List (List(..), index, (:))
import Data.List.NonEmpty (last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd)
import DataType (FieldName, cBarChart, cCons, cLineChart, cLinePlot, cLink, cMultiView, cNil, cParagraph, cScatterPlot, cText, f_caption, f_height, f_labels, f_legend, f_name, f_plots, f_points, f_segments, f_size, f_stackedBars, f_tickLabels, f_views, f_width, f_x, f_y, f_z)
import Partial.Unsafe (unsafePartial)
import Dict (Dict)
import Link (Link(..))
import Primitive (boolean, int, string, typeError)
import Primitive (unpack) as P
import Util (type (×), definitely, error, (×))
import Util.Map (get, mapWithKey)
import Val (BaseVal(..), DictRep(..), Val(..))

type Views =
   { multiViewEntry :: Int -> SelSetter Val Val
   , barChart_stackedBars :: SelSetter Val Val
   , lineChart_plots :: SelSetter Val Val
   , linePoint :: Int -> SelSetter Val Val
   , scatterPlot_points :: SelSetter Val Val
   , decodeBarChart :: Val (SelStates 𝕊) -> BarChart
   , decodeLineChart :: Val (SelStates 𝕊) -> LineChart
   , decodeScatterPlot :: Val (SelStates 𝕊) -> ScatterPlot
   }

-- Selectors and decoders for the view types, closed over the field positions
-- resolved from the class declarations in view.fld.
mkViews :: (Name -> FieldName -> Int) -> Views
mkViews ix =
   { multiViewEntry: \n -> listElement n >>> constrArg (last cMultiView) (ix cMultiView f_views)
   , barChart_stackedBars: constrArg (last cBarChart) (ix cBarChart f_stackedBars)
   , lineChart_plots: constrArg (last cLineChart) (ix cLineChart f_plots)
   , linePoint: \i -> listElement i >>> constrArg (last cLinePlot) (ix cLinePlot f_points)
   , scatterPlot_points: constrArg (last cScatterPlot) (ix cScatterPlot f_points)
   , decodeBarChart: \v -> unsafePartial (decBarChart v)
   , decodeLineChart: \v -> unsafePartial (decLineChart v)
   , decodeScatterPlot: \v -> unsafePartial (decScatterPlot v)
   }
   where
   arg :: Name -> FieldName -> List (Val (SelStates 𝕊)) -> Val (SelStates 𝕊)
   arg c f us = definitely "field index in range" (index us (ix c f))

   decBarChart :: Partial => Val (SelStates 𝕊) -> BarChart
   decBarChart (Val _ _ u) = case u of
      Constr c us | c == cBarChart -> BarChart
         { caption: P.unpack string (arg cBarChart f_caption us)
         , size: dict from (arg cBarChart f_size us)
         , tickLabels: dict from (arg cBarChart f_tickLabels us)
         , stackedBars: dict from <$> from (arg cBarChart f_stackedBars us)
         , legend: P.unpack boolean (arg cBarChart f_legend us)
         }
      _ -> typeError u "BarChart"

   decLineChart :: Partial => Val (SelStates 𝕊) -> LineChart
   decLineChart (Val _ _ u) = case u of
      Constr c us | c == cLineChart -> LineChart
         { size: dict from (arg cLineChart f_size us)
         , tickLabels: dict from (arg cLineChart f_tickLabels us)
         , caption: P.unpack string (arg cLineChart f_caption us)
         , plots: decLinePlot <$> (from (arg cLineChart f_plots us) :: Array (Val (SelStates 𝕊)))
         }
      _ -> typeError u "LineChart"

   decLinePlot :: Partial => Val (SelStates 𝕊) -> LinePlot
   decLinePlot (Val _ _ u) = case u of
      Constr c us | c == cLinePlot -> LinePlot
         { name: P.unpack string (arg cLinePlot f_name us)
         , points: dict from <$> from (arg cLinePlot f_points us)
         }
      _ -> typeError u "LinePlot"

   decScatterPlot :: Partial => Val (SelStates 𝕊) -> ScatterPlot
   decScatterPlot (Val _ _ u) = case u of
      Constr c us | c == cScatterPlot -> ScatterPlot
         { caption: P.unpack string (arg cScatterPlot f_caption us)
         , points: dict from <$> from (arg cScatterPlot f_points us)
         , labels: dict from (arg cScatterPlot f_labels us)
         }
      _ -> typeError u "ScatterPlot"

-- TODO: merge with 'view' below.
view' :: Partial => Views -> Options -> String -> Val (SelStates 𝕊) -> View
view' views options title v@(Val _ v_opt _) =
   pack $ DocView { doc: viewParagraph <$> v_opt, view: view views options title v }
   where
   viewParagraph (Val _ _ (Constr c (u : Nil))) | c == cParagraph =
      Paragraph (view views options "" <$> from u)

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
-- TODO: given the typeError clause, Partial no longer needed
view :: Partial => Views -> Options -> String -> Val (SelStates 𝕊) -> View
view views options title v@(Val α _ u') = case u' of
   Int n -> pack (Text (show n × α))
   Float n -> pack (Text (show n × α))
   Str str -> pack (Text (str × α))
   Constr c (u : Nil)
      | c == cText -> pack (from u :: Text)
      | c == cMultiView -> pack (MultiView (view views options "" <$> from u))
      | c == cParagraph -> pack (Paragraph (view views options "" <$> from u))
   Constr c (_ : _ : Nil)
      -- more consistent with other views for Link to take single argument of record type
      | c == cLink -> pack (from v :: Link)
   Constr c _
      | c == cBarChart -> pack (views.decodeBarChart v)
      | c == cScatterPlot -> pack (views.decodeScatterPlot v)
      | c == cLineChart -> pack (views.decodeLineChart v)
      | c == cNil || c == cCons ->
           if tableView then
              let
                 rowFilter = fromMaybe Interactive options.rowFilter
                 records = dict identity <$> vs
                 colNames = headers records
                 rows = arrayDictToArray2 colNames records <#> map snd
              in
                 pack (TableView { title, rowFilter, colNames, rows })
           else pack (MultiView $ view views options "" <$> vs)
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
   viewDict = mapWithKey \k (α' × v') -> pack (Text (k × α')) × view views options k v'

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

instance Reflect (Val (SelStates 𝕊)) Text where
   from (Val α _ v) = case v of
      Str s -> Text (s × α)
      _ -> typeError v "Text expects string"

instance Reflect (Val (SelStates 𝕊)) Link where
   from (Val _ _ u) = case u of
      (Constr c (v : (Val α' _ (Str s) : Nil))) | c == cLink -> Link v (s × α')
      _ -> typeError u "Link expects string as second argument"
