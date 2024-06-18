module App.View.ScatterPlot where

import Prelude

import App.Util (class Reflect, SelState, Selectable, 𝕊, ViewSelector, from, record)
import App.Util.Selector (field, listElement, scatterPlot)
import App.View.LineChart (Point)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import DataType (f_caption, f_data, f_xlabel, f_ylabel)
import Dict (Dict)
import Primitive (string, unpack)
import Util.Map (get)
import Val (Val)

newtype ScatterPlot = ScatterPlot
   { caption :: Selectable String
   , points :: Array Point
   , xlabel :: Selectable String
   , ylabel :: Selectable String
   }

foreign import drawScatterPlot :: Renderer ScatterPlot Unit

instance Drawable ScatterPlot Unit where
   draw divId suffix redraw view viewState =
      drawScatterPlot { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw scatterPlotSelector
      where
      scatterPlotSelector :: ViewSelector PointIndex
      scatterPlotSelector { i } = scatterPlot <<< field f_data <<< listElement i

instance Reflect (Dict (Val (SelState 𝕊))) ScatterPlot where
   from r = ScatterPlot
      { caption: unpack string (get f_caption r)
      , points: record from <$> from (get f_data r)
      , xlabel: unpack string (get f_xlabel r)
      , ylabel: unpack string (get f_ylabel r)
      }

type PointIndex = { i :: Int }
