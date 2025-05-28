module App.View.ScatterPlot where

import Prelude

import App.Util (Selectable, isPrimary, isSecondary)
import App.Util.Selector (ViewSelSetter, scatterPlot, scatterPoint)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import App.View.Util.Point (Point(..))
import Bind ((⟼))
import Data.Int (toNumber)
import Data.Tuple (snd)
import Foreign.Object (Object, fromFoldable)
import Lattice ((∨))
import Util ((!))

newtype ScatterPlot = ScatterPlot
   { caption :: Selectable String
   , points :: Array (Point Number)
   , labels :: Point String
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

type PointIndex = { i :: Int }
