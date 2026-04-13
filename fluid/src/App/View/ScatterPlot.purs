module App.View.ScatterPlot where

import Prelude

import App.Util (Selectable, isPrimary, isSecondary, selClasses, selClassesFor, selectionEventData')
import App.Util.Selector (ViewSelSetter, scatterPlot, scatterPoint)
import App.View.Util (class Viewable, Select, UIHelpers, registerMouseListeners, uiHelpers)
import App.View.Util.D3 as D3
import App.View.Util.Point (Point(..))
import Bind ((⟼))
import Data.Int (toNumber)
import Data.Tuple (snd, uncurry)
import Effect (Effect, foreachE)
import Foreign.Object (fromFoldable)
import Lattice ((∨))
import Util (type (×), (!))
import Web.Event.EventTarget (eventListener)

newtype ScatterPlot = ScatterPlot
   { caption :: Selectable String
   , points :: Array (Point Number)
   , labels :: Point String
   }

foreign import createElement :: UIHelpers -> ScatterPlot -> D3.Selection -> Effect D3.Selection

instance Viewable ScatterPlot Unit where
   isLeaf = const false
   createElement _ = createElement uiHelpers
   setSelection _ (ScatterPlot { points }) select rootElement = do
      listener <- eventListener (select <<< uncurry scatterPlotPoint <<< selectionEventData')
      pointEls <- D3.selectAll ".scatterplot-point" rootElement
      foreachE pointEls \pointEl -> do
         idx :: PointIndex <- D3.datum pointEl
         let
            Point { x, y } = points ! idx.i
            sel = snd x ∨ snd y
         void $ D3.classed selClasses false pointEl
         void $ D3.classed (selClassesFor sel) true pointEl
         void $ D3.attrs pointEl (fromFoldable (pointAttrs points idx))
         registerMouseListeners listener pointEl

scatterPlotPoint :: ViewSelSetter PointIndex
scatterPlotPoint { i } = scatterPoint i >>> scatterPlot

pointAttrs :: Array (Point Number) -> PointIndex -> Array (String × String)
pointAttrs points { i } =
   [ "r" ⟼ toNumber pointSmallRadius * if isPrimary sel then 1.6 else if isSecondary sel then 1.25 else 1.0 ]
   where
   Point { x, y } = points ! i
   sel = snd x ∨ snd y
   pointSmallRadius = 2

type PointIndex = { i :: Int }
