module App.View.ScatterPlot where

import Prelude

import App.Util (class Reflect, Handler, Renderer, SelState, Selectable, Selector, 𝕊, from, record, selector, unsafeEventData)
import App.Util.Selector (field, listElement, scatterPlot)
import App.View.LineChart (Point)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import DataType (f_caption, f_data, f_xlabel, f_ylabel)
import Dict (Dict)
import Primitive (string, unpack)
import Util (type (×), Endo, (!), (×))
import Util.Map (get)
import Val (Val)
import Web.Event.Event (EventType, target, type_)
import Web.Event.Internal.Types (EventTarget)

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

scatterPlotHandler :: Handler
scatterPlotHandler = (target &&& type_) >>> index >>> uncurry togglePoint
   where
   togglePoint :: Int -> Endo (Selector Val)
   togglePoint i = scatterPlot <<< field f_data <<< listElement i

   index :: Maybe EventTarget × EventType -> Int × Selector Val
   index (tgt_opt × ty) = unsafeEventData tgt_opt ! 0 × selector ty
