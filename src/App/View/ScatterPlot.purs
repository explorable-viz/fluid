module App.View.ScatterPlot where

import Prelude

import App.Util (class Reflect, Handler, Renderer, SelState, Selectable, Selector, 𝕊, from, record, eventData)
import App.Util.Selector (field, listElement, scatterPlot)
import App.View.LineChart (Point)
import Data.Tuple (uncurry)
import DataType (f_caption, f_data, f_xlabel, f_ylabel)
import Dict (Dict)
import Primitive (string, unpack)
import Util (Endo)
import Util.Map (get)
import Val (Val)

newtype ScatterPlot = ScatterPlot
   { caption :: Selectable String
   , data :: Array Point
   , xlabel :: Selectable String
   , ylabel :: Selectable String
   }

foreign import drawScatterPlot :: Renderer ScatterPlot

instance Reflect (Dict (Val (SelState 𝕊))) ScatterPlot where
   from r = ScatterPlot
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
      , xlabel: unpack string (get f_xlabel r)
      , ylabel: unpack string (get f_ylabel r)
      }

type PointIndex = Int

scatterPlotHandler :: Handler
scatterPlotHandler = eventData >>> uncurry togglePoint
   where
   togglePoint :: Int -> Endo (Selector Val)
   togglePoint i = scatterPlot <<< field f_data <<< listElement i
