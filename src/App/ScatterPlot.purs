module App.ScatterPlot where

import Prelude

import App.Util (class Reflect, Renderer, Handler, from, get_intOrNumber, record)
import App.Util.Select (constrArg, field, listElement)
import Data.Maybe (Maybe)
import DataType (cScatterPlot, f_caption, f_data, f_x, f_xlabel, f_y, f_ylabel)
import Dict (Dict, get)
import Lattice (𝔹, neg)
import Primitive (string, unpack)
import Test.Util (Selector)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), definitely', (!))
import Val (Val)
import Web.Event.Event (target)
import Web.Event.Internal.Types (EventTarget)

newtype ScatterPlot = ScatterPlot
   { caption :: String × 𝔹
   , data :: Array ScatterRecord
   , xlabel :: String × 𝔹
   , ylabel :: String × 𝔹
   }

newtype ScatterRecord = ScatterRecord
   { x :: Number × 𝔹
   , y :: Number × 𝔹
   }

foreign import drawScatterPlot :: Renderer ScatterPlot

instance Reflect (Dict (Val 𝔹)) ScatterRecord where
   from r = ScatterRecord
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (Val 𝔹)) ScatterPlot where
   from r = ScatterPlot
     { caption: unpack string (get f_caption r)
     , data: record from <$> from (get f_data r)
     , xlabel: unpack string (get f_xlabel r)
     , ylabel: unpack string (get f_ylabel r)
     }

scatterPlotHandler :: Handler
scatterPlotHandler ev = toggleDot $ unsafeDotIndex $ target ev
   where
   toggleDot :: Int -> Selector Val
   toggleDot i = 
      constrArg cScatterPlot 0
         $ field f_data
         $ listElement i
         $ neg
   
   unsafeDotIndex :: Maybe EventTarget -> Int
   unsafeDotIndex tgt_opt =
      let
         tgt = definitely' $ tgt_opt
      in
         (unsafeCoerce tgt).__data__ ! 0