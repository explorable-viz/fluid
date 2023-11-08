module App.BubbleChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Renderer, Handler, from, get_intOrNumber, record)
import App.Util.Select (constrArg, field, listElement)
import Data.Maybe (Maybe)
import DataType (cBubbleChart, f_caption, f_data, f_x, f_y, f_z)
import Dict (Dict, get)
import Lattice (𝔹, neg)
import Primitive (string)
import Test.Util (Selector)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), definitely', (!))
import Val (Val)
import Web.Event.Event (target)
import Web.Event.Internal.Types (EventTarget)

newtype BubbleChart = BubbleChart { caption :: String × 𝔹, data :: Array BubbleChartRecord}
newtype BubbleChartRecord = BubbleChartRecord { x :: Number × 𝔹, y :: Number × 𝔹, z :: Number × 𝔹 }

foreign import drawBubbleChart :: Renderer BubbleChart

instance Reflect (Dict (Val 𝔹)) BubbleChartRecord where
   from r = BubbleChartRecord
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      , z: get_intOrNumber f_z r 
      }
instance Reflect (Dict (Val 𝔹)) BubbleChart where
   from r = BubbleChart
      { caption: string.unpack (get f_caption r)
      , data: record from <$> from (get f_data r)
      }

bubbleChartHandler :: Handler
bubbleChartHandler ev = toggleDot $ unsafeDotIndex $ target ev
   where
   toggleDot :: Int -> Selector Val
   toggleDot i = 
      constrArg cBubbleChart 0
         $ field f_data
         $ listElement i
         $ neg
   unsafeDotIndex :: Maybe EventTarget -> Int
   unsafeDotIndex tgt_opt = 
      let
         tgt = definitely' $ tgt_opt
      in
         (unsafeCoerce tgt).__data__ ! 0