module App.View.ScatterPlot where

import Prelude

import App.Util (Selectable, isPrimary, isSecondary, selectionEventData')
import App.Util.Selector (ViewSelSetter, scatterPlot, scatterPoint)
import App.View.Util (class View2, Select, UIHelpers, uiHelpers)
import App.View.Util.D3 as D3
import App.View.Util.Point (Point(..))
import Bind ((⟼))
import Data.Int (toNumber)
import Data.Tuple (snd, uncurry)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Lattice ((∨))
import Util ((!))
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)

newtype ScatterPlot = ScatterPlot
   { caption :: Selectable String
   , points :: Array (Point Number)
   , labels :: Point String
   }

type ScatterPlotHelpers =
   { point_attrs :: ScatterPlot -> PointIndex -> Object String
   , eventListener :: (Event -> Effect Unit) -> Effect EventListener
   , withScatterPlotPoint :: Select -> (Event -> Effect Unit)
   }

foreign import createElement :: UIHelpers -> ScatterPlot -> D3.Selection -> Effect D3.Selection
foreign import setSelection :: ScatterPlotHelpers -> UIHelpers -> ScatterPlot -> Select -> D3.Selection -> Effect Unit

instance View2 ScatterPlot Unit where
   createElement _ = createElement uiHelpers
   setSelection _ = setSelection scatterPlotHelpers uiHelpers

scatterPlotHelpers :: ScatterPlotHelpers
scatterPlotHelpers =
   { point_attrs
   , eventListener
   , withScatterPlotPoint
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

   scatterPlotPoint :: ViewSelSetter PointIndex
   scatterPlotPoint { i } = scatterPoint i >>> scatterPlot
   withScatterPlotPoint sel = sel <<< uncurry scatterPlotPoint <<< selectionEventData'

type PointIndex = { i :: Int }
