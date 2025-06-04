module App.View.ScatterPlot where

import Prelude

import App.Util (Selectable, isPrimary, isSecondary)
import App.Util.Selector (ViewSelSetter, scatterPlot, scatterPoint)
import App.View.Util (class Drawable, class Drawable2, UIHelpers, draw', selListener, uiHelpers)
import App.View.Util.D3 as D3
import App.View.Util.Point (Point(..))
import Bind ((⟼))
import Data.Int (toNumber)
import Data.Tuple (snd)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Lattice ((∨))
import Util ((!))
import Web.Event.EventTarget (EventListener)

newtype ScatterPlot = ScatterPlot
   { caption :: Selectable String
   , points :: Array (Point Number)
   , labels :: Point String
   }

type ScatterPlotHelpers =
   { point_attrs :: ScatterPlot -> PointIndex -> Object String
   }

foreign import createRootElement2 :: UIHelpers -> ScatterPlot -> D3.Selection -> String -> Effect D3.Selection
foreign import setSelStates2 :: ScatterPlotHelpers -> UIHelpers -> ScatterPlot -> EventListener -> D3.Selection -> Effect Unit

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
      draw' uiHelpers rSpec =<< selListener figVal redraw scatterPlotPoint
      where
      scatterPlotPoint :: ViewSelSetter PointIndex
      scatterPlotPoint { i } = scatterPoint i >>> scatterPlot

instance Drawable2 ScatterPlot where
   createRootElement = createRootElement2 uiHelpers
   setSelStates = setSelStates2 scatterPlotHelpers uiHelpers

type PointIndex = { i :: Int }
