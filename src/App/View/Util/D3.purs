module App.View.Util.D3 where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)
import Util (Endo)

-- d3.js ticks are actually (start, stop, count) but we only supply first argument
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

foreign import data D3Selection :: Type

foreign import createChild :: D3Selection -> String -> Object String -> Effect D3Selection
foreign import createChildren :: forall a. D3Selection -> String -> Array a -> Object (a -> String) -> Effect D3Selection
foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number
foreign import xAxis :: Coord (Endo Number) -> Coord Ticks -> D3Selection -> Effect Unit
foreign import yAxis :: Coord (Endo Number) -> Coord Ticks -> D3Selection -> Effect Unit
foreign import textWidth :: String -> Int
foreign import line :: Coord (Endo Number) -> Array (Coord Number) -> Effect String
foreign import text :: String -> D3Selection -> Effect Unit
