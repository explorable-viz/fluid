module App.View.ScatterPlot where

import Prelude

import App.Util (class Reflect, Handler, Renderer, Sel, from, record, unsafeEventData)
import App.Util.Selector (constrArg, field, listElement)
import App.View.LineChart (Point)
import Data.Maybe (Maybe)
import DataType (cScatterPlot, f_caption, f_data, f_xlabel, f_ylabel)
import Dict (Dict)
import Lattice (neg)
import Primitive (string, unpack)
import Test.Util (Selector)
import Util (type (×), (!))
import Util.Map (get)
import Val (Val)
import Web.Event.Event (target)
import Web.Event.Internal.Types (EventTarget)

newtype ScatterPlot = ScatterPlot
   { caption :: String × Sel
   , data :: Array Point
   , xlabel :: String × Sel
   , ylabel :: String × Sel
   }

foreign import drawScatterPlot :: Renderer ScatterPlot

instance Reflect (Dict (Val Sel)) ScatterPlot where
   from r = ScatterPlot
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
      , xlabel: unpack string (get f_xlabel r)
      , ylabel: unpack string (get f_ylabel r)
      }

scatterPlotHandler :: Handler
scatterPlotHandler = target >>> index >>> togglePoint
   where
   togglePoint :: Int -> Selector Val
   togglePoint i =
      constrArg cScatterPlot 0
         $ field f_data
         $ listElement i
         $ neg

   index :: Maybe EventTarget -> Int
   index tgt_opt = unsafeEventData tgt_opt ! 0
