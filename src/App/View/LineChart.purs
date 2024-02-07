module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, Sel, Selectable, from, get_intOrNumber, record, unsafeEventData)
import App.Util.Selector (constrArg, field, linePoint, listElement)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import DataType (cLineChart, cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Dict (Dict)
import Lattice (neg)
import Primitive (string, unpack)
import Test.Util (Selector)
import Util (type (×), (×), (!))
import Util.Map (get)
import Val (BaseVal(..), Val(..))
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)

newtype LineChart = LineChart
   { caption :: Selectable String
   , plots :: Array LinePlot
   }

newtype LinePlot = LinePlot
   { name :: Selectable String
   , data :: Array Point
   }

newtype Point = Point
   { x :: Selectable Number
   , y :: Selectable Number
   }

foreign import drawLineChart :: Renderer LineChart

instance Reflect (Dict (Val Sel)) Point where
   from r = Point
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (Val Sel)) LinePlot where
   from r = LinePlot
      { name: unpack string (get f_name r)
      , data: record from <$> from (get f_data r)
      }

instance Reflect (Dict (Val Sel)) LineChart where
   from r = LineChart
      { caption: unpack string (get f_caption r)
      , plots: from <$> (from (get f_plots r) :: Array (Val Sel)) :: Array LinePlot
      }

instance Reflect (Val Sel) LinePlot where
   from (Val _ (Constr c (u1 : Nil))) | c == cLinePlot = record from u1

lineChartHandler :: Handler
lineChartHandler = target >>> pos >>> togglePoint
   where
   togglePoint :: Int × Int -> Selector Val
   togglePoint (i × j) =
      constrArg cLineChart 0
         $ field f_plots
         $ listElement i
         $ linePoint j
         $ neg

   -- [Unsafe] 0-based indices of line plot and point within line plot.
   pos :: Maybe EventTarget -> Int × Int
   pos tgt_opt = xy ! 0 × xy ! 1
      where
      xy = unsafeEventData tgt_opt ! 0 :: Array Int
