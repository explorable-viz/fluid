module App.View.ScatterPlot where

import Prelude

import App.Util (class Reflect, SelState, Selectable, 𝕊, dict, from, isPrimary, isSecondary)
import App.Util.Selector (ViewSelSetter, scatterPlot, scatterPoint)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import App.View.Util.Point (Point(..))
import Bind ((⟼))
import Data.Int (toNumber)
import Data.Tuple (snd)
import DataType (f_caption, f_points, f_xlabel, f_ylabel)
import Dict (Dict)
import Foreign.Object (Object, fromFoldable)
import Lattice ((∨))
import Primitive (string, unpack)
import Util (type (×), (!))
import Util.Map (get)
import Val (Val)

newtype ScatterPlot = ScatterPlot
   { caption :: Selectable String
   , points :: Array (Point Number)
   , xlabel :: Selectable String
   , ylabel :: Selectable String
   }

type ScatterPlotHelpers =
   { point_attrs :: ScatterPlot -> PointIndex -> Object String
   }

foreign import drawScatterPlot :: ScatterPlotHelpers -> Renderer ScatterPlot

scatterPlotHelpers :: ScatterPlotHelpers
scatterPlotHelpers =
   { point_attrs
   }
   where
   point_attrs :: ScatterPlot -> PointIndex -> Object String
   point_attrs (ScatterPlot { points }) { i } =
      fromFoldable
         [ "r" ⟼ toNumber point_smallRadius * if isPrimary sel then 1.6 else if isSecondary sel then 1.25 else 1.0 ]
      where
      Point { x, y } = points ! i
      sel = snd x ∨ snd y
      point_smallRadius = 2

instance Drawable ScatterPlot where
   draw rSpec figVal _ redraw =
      drawScatterPlot scatterPlotHelpers uiHelpers rSpec =<< selListener figVal redraw point
      where
      point :: ViewSelSetter PointIndex
      point { i } = scatterPoint i >>> scatterPlot

instance Reflect (Dict (SelState 𝕊 × Val (SelState 𝕊))) ScatterPlot where
   from r = ScatterPlot
      { caption: unpack string (snd (get f_caption r))
      , points: dict from <$> from (snd (get f_points r))
      , xlabel: unpack string (snd (get f_xlabel r))
      , ylabel: unpack string (snd (get f_ylabel r))
      }

type PointIndex = { i :: Int }
