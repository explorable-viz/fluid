module App.View where

import Prelude hiding (absurd)

import App.Util (Dimensions(..), SelStates, Selectable, 𝕊, dict, unpackIntOrNumber)
import App.View.BarChart (BarChart(..))
import App.View.ViewWithDoc (ViewWithDoc)
import App.View.LineChart (LineChart(..), LinePlot(..))
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.MultiView (MultiView(..))
import App.View.Paragraph (Paragraph(..))
import App.View.ScatterPlot (ScatterPlot(..))
import App.View.Segment (Segment(..))
import App.View.StackedBar (StackedBar(..))
import App.View.TableView (TableView(..), arrayDictToArray2, headers)
import App.View.Text (Text(..))
import App.View.Util (Filter(..), View, pack)
import App.View.Util.Axes (Orientation, orientation)
import App.View.Util.Point (Point(..))
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd)
import DataType (ClassTable, cAxisLabels, cBarChart, cDimensions, cLineChart, cLinePlot, cLink, cMultiView, cParagraph, cPoint, cScatterPlot, cSegment, cStackedBar, cText, cTickLabels, f_caption, f_fragments, f_height, f_label, f_labels, f_legend, f_name, f_plots, f_points, f_segments, f_size, f_stackedBars, f_text, f_tickLabels, f_value, f_views, f_width, f_x, f_y, f_z, fieldIndex, fieldNames)
import Dict (Dict)
import Link (Link(..))
import Literal (Literal(..))
import Pretty (prettyP)
import Primitive (boolean, int, string, typeError)
import Primitive (unpack') as P
import Util (type (×), absurd, error, head, nonEmpty, (!), (×))
import Util.Map (mapWithKey)
import Val (BaseVal(..), DictRep(..), Val(..), ValWithDoc(..))

type ViewOptions = { classes :: ClassTable, rowFilter :: Maybe Filter }

-- TODO: merge with 'view' below.
viewWithDoc :: ViewOptions -> String -> ValWithDoc (SelStates 𝕊) -> ViewWithDoc
viewWithDoc options title (ValWithDoc { val: v, doc }) =
   { doc: reflect options <$> doc :: Maybe Paragraph, view: view options title v }

-- Convert annotated value to appropriate view, discarding top-level annotations for now.
view :: ViewOptions -> String -> Val (SelStates 𝕊) -> View
view options title v@(Val α u') = case u' of
   Lit (Str str) -> pack (Text (str × α))
   Lit ℓ -> pack (Text (prettyP ℓ × α))
   Constr c _
      | c == cText -> pack (reflect options v :: Text)
      | c == cMultiView -> pack (reflect options v :: MultiView)
      | c == cParagraph -> pack (reflect options v :: Paragraph)
      | c == cLink -> pack (reflect options v :: Link)
      | c == cBarChart -> pack (reflect options v :: BarChart)
      | c == cScatterPlot -> pack (reflect options v :: ScatterPlot)
      | c == cLineChart -> pack (reflect options v :: LineChart)
   List vs -> case head vs of
      Just (Val _ (Constr c _)) | not (c `A.elem` viewClasses) ->
         pack (TableView { title, rowFilter, className: Just c, colNames: A.fromFoldable (fieldNames options.classes c), rows: vs <#> fieldValues })
      Just (Val _ (Constr _ _)) -> pack (MultiView $ view options "" <$> vs)
      _ ->
         let
            records = dict identity <$> vs
            colNames = headers records
            rows = arrayDictToArray2 colNames records <#> map snd
         in
            pack (TableView { title, rowFilter, className: Nothing, colNames, rows })
      where
      rowFilter = fromMaybe Interactive options.rowFilter
      viewClasses = [ cText, cMultiView, cParagraph, cLink, cBarChart, cScatterPlot, cLineChart ]
      fieldValues (Val _ (Constr _ us)) = A.fromFoldable us
      fieldValues _ = error absurd
   Matrix r ->
      pack (MatrixView { title, matrix: matrixRep r })
   Dictionary (DictRep d) ->
      pack (viewDict d)
   _ -> typeError u' "Viewable"
   where
   viewDict :: Dict (SelStates 𝕊 × Val (SelStates 𝕊)) -> Dict (View × View)
   viewDict = mapWithKey \k (α' × v') -> pack (Text (k × α')) × view options k v'

class Reflect b where
   reflect :: ViewOptions -> Val (SelStates 𝕊) -> b

instance Reflect (Val (SelStates 𝕊)) where
   reflect _ = identity

instance Reflect b => Reflect (Array b) where
   reflect options (Val _ (List vs)) = reflect options <$> vs
   reflect _ (Val _ u) = typeError u "list"

instance Reflect b => Reflect (NonEmptyArray b) where
   reflect options v = nonEmpty (reflect options v)

instance Reflect BarChart where
   reflect options@{ classes } (Val _ u) = case u of
      Constr c us | c == cBarChart -> BarChart
         { caption: P.unpack' string (us ! fieldIndex classes cBarChart f_caption)
         , size: reflect options (us ! fieldIndex classes cBarChart f_size)
         , tickLabels: reflect options (us ! fieldIndex classes cBarChart f_tickLabels)
         , stackedBars: reflect options (us ! fieldIndex classes cBarChart f_stackedBars)
         , legend: P.unpack' boolean (us ! fieldIndex classes cBarChart f_legend)
         }
      _ -> typeError u "BarChart"

instance Reflect LineChart where
   reflect options@{ classes } (Val _ u) = case u of
      Constr c us | c == cLineChart -> LineChart
         { size: reflect options (us ! fieldIndex classes cLineChart f_size)
         , tickLabels: reflect options (us ! fieldIndex classes cLineChart f_tickLabels)
         , caption: P.unpack' string (us ! fieldIndex classes cLineChart f_caption)
         , plots: reflect options (us ! fieldIndex classes cLineChart f_plots)
         }
      _ -> typeError u "LineChart"

instance Reflect LinePlot where
   reflect options@{ classes } (Val _ u) = case u of
      Constr c us | c == cLinePlot -> LinePlot
         { name: P.unpack' string (us ! fieldIndex classes cLinePlot f_name)
         , points: reflect options (us ! fieldIndex classes cLinePlot f_points)
         }
      _ -> typeError u "LinePlot"

instance Reflect ScatterPlot where
   reflect options@{ classes } (Val _ u) = case u of
      Constr c us | c == cScatterPlot -> ScatterPlot
         { caption: P.unpack' string (us ! fieldIndex classes cScatterPlot f_caption)
         , points: reflect options (us ! fieldIndex classes cScatterPlot f_points)
         , labels: reflect options (us ! fieldIndex classes cScatterPlot f_labels)
         }
      _ -> typeError u "ScatterPlot"

instance Reflect (Dimensions (Selectable Int)) where
   reflect { classes } (Val _ u) = case u of
      Constr c us | c == cDimensions -> Dimensions
         { width: P.unpack' int (us ! fieldIndex classes cDimensions f_width)
         , height: P.unpack' int (us ! fieldIndex classes cDimensions f_height)
         }
      _ -> typeError u "Dimensions"

instance Reflect (Point Number) where
   reflect { classes } (Val _ u) = case u of
      Constr c us | c == cPoint -> Point
         { x: unpackIntOrNumber (us ! fieldIndex classes cPoint f_x)
         , y: unpackIntOrNumber (us ! fieldIndex classes cPoint f_y)
         }
      _ -> typeError u "Point"

instance Reflect (Point String) where
   reflect { classes } (Val _ u) = case u of
      Constr c us | c == cAxisLabels -> Point
         { x: P.unpack' string (us ! fieldIndex classes cAxisLabels f_x)
         , y: P.unpack' string (us ! fieldIndex classes cAxisLabels f_y)
         }
      _ -> typeError u "AxisLabels"

instance Reflect (Point Orientation) where
   reflect { classes } (Val _ u) = case u of
      Constr c us | c == cTickLabels -> Point
         { x: P.unpack' orientation (us ! fieldIndex classes cTickLabels f_x)
         , y: P.unpack' orientation (us ! fieldIndex classes cTickLabels f_y)
         }
      _ -> typeError u "TickLabels"

instance Reflect Segment where
   reflect { classes } (Val _ u) = case u of
      Constr c us | c == cSegment -> Segment
         { y: P.unpack' string (us ! fieldIndex classes cSegment f_y)
         , z: unpackIntOrNumber (us ! fieldIndex classes cSegment f_z)
         }
      _ -> typeError u "Segment"

instance Reflect StackedBar where
   reflect options@{ classes } (Val _ u) = case u of
      Constr c us | c == cStackedBar -> StackedBar
         { x: P.unpack' string (us ! fieldIndex classes cStackedBar f_x)
         , segments: reflect options (us ! fieldIndex classes cStackedBar f_segments)
         }
      _ -> typeError u "StackedBar"

instance Reflect Text where
   reflect { classes } (Val _ u) = case u of
      Constr c us | c == cText -> case us ! fieldIndex classes cText f_text of
         Val α (Lit (Str s)) -> Text (s × α)
         _ -> typeError u "Text expects string"
      _ -> typeError u "Text"

instance Reflect Link where
   reflect { classes } (Val _ u) = case u of
      Constr c us | c == cLink -> case us ! fieldIndex classes cLink f_label of
         Val α' (Lit (Str s)) -> Link (us ! fieldIndex classes cLink f_value) (s × α')
         _ -> typeError u "Link expects string label"
      _ -> typeError u "Link"

instance Reflect MultiView where
   reflect options@{ classes } (Val _ u) = case u of
      Constr c us | c == cMultiView -> MultiView (view options "" <$> reflect options (us ! fieldIndex classes cMultiView f_views))
      _ -> typeError u "MultiView"

instance Reflect Paragraph where
   reflect options@{ classes } (Val _ u) = case u of
      Constr c us | c == cParagraph -> Paragraph (view options "" <$> reflect options (us ! fieldIndex classes cParagraph f_fragments))
      _ -> typeError u "Paragraph"
