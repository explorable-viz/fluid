module App.View.Util.D3 where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)
import Util (Endo)

-- Documentation says d3.ticks takes (start, stop, count) but could only make it work with count
type Ticks = Number

type Margin =
   { top :: Int
   , right :: Int
   , bottom :: Int
   , left :: Int
   }

type Coord a =
   { x :: a
   , y :: a
   }

type Dimensions =
   { width :: Int
   , height :: Int
   }

textWidth :: String -> Int
textWidth = textDimensions >>> _.width

textHeight :: String -> Int
textHeight = textDimensions >>> _.height

foreign import data D3Selection :: Type

foreign import createChild :: D3Selection -> String -> Object String -> Effect D3Selection
foreign import createChildren :: forall a. D3Selection -> String -> String -> Array a -> Object (a -> String) -> Effect D3Selection
foreign import remove :: D3Selection -> Effect Unit
foreign import nameCol :: String -> Array String -> String
foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number
foreign import xAxis :: Coord (Endo Number) -> Coord Ticks -> D3Selection -> Effect D3Selection
foreign import yAxis :: Coord (Endo Number) -> Coord Ticks -> D3Selection -> Effect D3Selection
foreign import textDimensions :: String -> Dimensions
foreign import line :: Coord (Endo Number) -> Array (Coord Number) -> String
foreign import text :: String -> D3Selection -> Effect Unit
