module App.BarChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, Selector, Selector2, Handler2, from, get_intOrNumber, record, selectAll, selectConstrArg, selectField, selectNth, selectNth2, toggleConstrArg, toggleField)
import Data.Maybe (Maybe)
import DataType (cBarChart, f_caption, f_data, f_x, f_y)
import Dict (Dict, get)
import Lattice (𝔹, neg)
import Primitive (string)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (!), definitely')
import Val (Val)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)

newtype BarChart = BarChart { caption :: String × 𝔹, data :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }

foreign import drawBarChart :: Renderer BarChart

instance Reflect (Dict (Val Boolean)) BarChartRecord where
   from r = BarChartRecord
      { x: string.match (get f_x r)
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (Val Boolean)) BarChart where
   from r = BarChart
      { caption: string.match (get f_caption r)
      , data: record from <$> from (get f_data r)
      }

barChartHandler :: Handler
barChartHandler ev = toggleBar $ unsafeBarIndex $ target ev
   where
   toggleBar :: Int -> Selector Val
   toggleBar i =
      toggleConstrArg cBarChart 0
         $ toggleField f_data
         $ selectNth i
         $ neg

barChartHandler2 :: Handler2
barChartHandler2 ev = toggleBar $ unsafeBarIndex $ target ev
   where
   toggleBar :: Int -> Selector2 Val
   toggleBar i =
      neg
         $ selectConstrArg cBarChart 0
         $ selectField f_data
         $ selectNth2 i
         $ selectAll

-- [Unsafe] Datum associated with bar chart mouse event; 0-based index of selected bar.
unsafeBarIndex :: Maybe EventTarget -> Int
unsafeBarIndex tgt_opt =
   let
      tgt = definitely' $ tgt_opt
   in
      (unsafeCoerce tgt).__data__ ! 0
