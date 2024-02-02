module App.View.BubbleChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, Sel, Selectable, from, get_intOrNumber, record, unsafeEventData)
import App.Util.Selector (constrArg, field, listElement)
import Data.Maybe (Maybe)
import DataType (cBubbleChart, f_caption, f_colour, f_data, f_x, f_xlabel, f_y, f_ylabel, f_z)
import Dict (Dict)
import Lattice (neg)
import Primitive (string, unpack)
import Test.Util (Selector)
import Util ((!))
import Util.Map (get)
import Val (Val)
import Web.Event.Event (target)
import Web.Event.Internal.Types (EventTarget)

newtype BubbleChart = BubbleChart
   { caption :: Selectable String
   , data :: Array BubbleChartRecord
   , xlabel :: Selectable String
   , ylabel :: Selectable String
   }

newtype BubbleChartRecord = BubbleChartRecord
   { x :: Selectable Number
   , y :: Selectable Number
   , z :: Selectable Number
   , c :: Selectable String
   }

foreign import drawBubbleChart :: Renderer BubbleChart

instance Reflect (Dict (Val Sel)) BubbleChartRecord where
   from r = BubbleChartRecord
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      , z: get_intOrNumber f_z r
      , c: unpack string $ get f_colour r
      }

instance Reflect (Dict (Val Sel)) BubbleChart where
   from r = BubbleChart
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
      , xlabel: unpack string (get f_xlabel r)
      , ylabel: unpack string (get f_ylabel r)
      }

bubbleChartHandler :: Handler
bubbleChartHandler = target >>> bubbleIndex >>> toggleBubble
   where
   toggleBubble :: Int -> Selector Val
   toggleBubble i =
      constrArg cBubbleChart 0
         $ field f_data
         $ listElement i
         $ neg

   bubbleIndex :: Maybe EventTarget -> Int
   bubbleIndex tgt_opt = unsafeEventData tgt_opt ! 0
