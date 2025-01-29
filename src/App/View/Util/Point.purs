module App.View.Util.Point where

import App.Util (class Reflect, SelState, Selectable, 𝕊, get_intOrNumber)
import App.View.Util.Axes (Orientation, orientation)
import App.View.Util.D3 (Coord)
import Data.Newtype (class Newtype)
import Data.Tuple (snd)
import DataType (f_x, f_y)
import Dict (Dict)
import Primitive (string, unpack)
import Util (type (×))
import Util.Map (get)
import Val (Val)

newtype Point a = Point (Coord (Selectable a))

-- ======================
-- boilerplate
-- ======================

derive instance Newtype (Point a) _

instance Reflect (Dict (SelState 𝕊 × Val (SelState 𝕊))) (Point Number) where
   from r = Point
      { x: get_intOrNumber f_x r
      , y: get_intOrNumber f_y r
      }

instance Reflect (Dict (SelState 𝕊 × Val (SelState 𝕊))) (Point String) where
   from r = Point
      { x: unpack string (snd (get f_x r))
      , y: unpack string (snd (get f_y r))
      }

instance Reflect (Dict (SelState 𝕊 × Val (SelState 𝕊))) (Point Orientation) where
   from r = Point
      { x: unpack orientation (snd (get f_x r))
      , y: unpack orientation (snd (get f_y r))
      }
