module App.View.LineChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, Sel, from, get_intOrNumber, record)
import App.Util.Selector (constrArg, field, listElement)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import DataType (cLineChart, cLinePlot, f_caption, f_data, f_name, f_plots, f_x, f_y)
import Dict (Dict, get)
import Lattice (neg)
import Primitive (string, unpack)
import Test.Util (Selector)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (×), (!), definitely')
import Val (BaseVal(..), Val(..))
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)

newtype LineChart = LineChart { caption :: String × Sel, plots :: Array LinePlot }
newtype LinePlot = LinePlot { name :: String × Sel, data :: Array Point }
newtype Point = Point { x :: Number × Sel, y :: Number × Sel }

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
lineChartHandler ev = togglePoint $ unsafePos $ target ev
   where
   togglePoint :: Int × Int -> Selector Val
   togglePoint (i × j) =
      constrArg cLineChart 0
         $ field f_plots
         $ listElement i
         $ constrArg cLinePlot 0
         $ field f_data
         $ listElement j
         $ neg

   -- [Unsafe] Datum associated with line-chart mouse event; 0-based indices of line plot and point
   -- within line plot.
   unsafePos :: Maybe EventTarget -> Int × Int
   unsafePos tgt_opt =
      let
         tgt = definitely' $ tgt_opt
         xy = (unsafeCoerce tgt).__data__ ! 0 :: Array Int
      in
         xy ! 0 × xy ! 1
