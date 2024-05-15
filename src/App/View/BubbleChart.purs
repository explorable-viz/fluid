module App.View.BubbleChart where

import Prelude hiding (absurd)

import App.Util (class Reflect, Handler, Renderer, SelState, Selectable, Selector, 𝕊, from, get_intOrNumber, record, selector, unsafeEventData)
import App.Util.Selector (bubbleChart, field, listElement)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import DataType (f_caption, f_colour, f_data, f_x, f_xlabel, f_y, f_ylabel, f_z)
import Dict (Dict)
import Primitive (string, unpack)
import Util (type (×), Endo, (!), (×))
import Util.Map (get)
import Val (Val)
import Web.Event.Event (EventType, target, type_)
import Web.Event.Internal.Types (EventTarget)

newtype BubbleChart = BubbleChart
   { caption :: Selectable String
   , data :: Array Bubble
   , xlabel :: Selectable String
   , ylabel :: Selectable String
   }

newtype Bubble = Bubble
   { x :: Selectable Number
   , y :: Selectable Number
   , z :: Selectable Number
   , c :: Selectable String
   }

foreign import drawBubbleChart :: Renderer BubbleChart

instance Reflect (Dict (Val (SelState 𝕊))) Bubble where
   from r = Bubble
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      , z: get_intOrNumber f_z r
      , c: unpack string $ get f_colour r
      }

instance Reflect (Dict (Val (SelState 𝕊))) BubbleChart where
   from r = BubbleChart
      { caption: unpack string (get f_caption r)
      , data: record from <$> from (get f_data r)
      , xlabel: unpack string (get f_xlabel r)
      , ylabel: unpack string (get f_ylabel r)
      }

bubbleChartHandler :: Handler
bubbleChartHandler = (target &&& type_) >>> bubbleIndex >>> uncurry toggleBubble
   where
   toggleBubble :: Int -> Endo (Selector Val)
   toggleBubble i = bubbleChart <<< field f_data <<< listElement i

   bubbleIndex :: Maybe EventTarget × EventType -> Int × Selector Val
   bubbleIndex (tgt_opt × ty) = unsafeEventData tgt_opt ! 0 × selector ty
