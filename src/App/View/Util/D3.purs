module App.View.Util.D3 where

import Prelude

import App.Util (Dimensions)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Newtype (unwrap)
import Effect (Effect)
import Foreign.Object (Object)
import Util (Endo)

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

textWidth :: String -> Int
textWidth = textDimensions >>> unwrap >>> _.width

textHeight :: String -> Int
textHeight = textDimensions >>> unwrap >>> _.height

foreign import data D3Selection :: Type

foreign import createChild :: D3Selection -> String -> Object String -> Effect D3Selection
foreign import createChildren :: forall a. D3Selection -> String -> String -> Array a -> Object (a -> String) -> Effect D3Selection
foreign import remove :: D3Selection -> Effect Unit
foreign import nameCol :: String -> Array String -> String
foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number
-- Currently have two different protocols for x and y axis -- will subsume into something more general
foreign import xAxis :: Coord (Endo Number) -> NonEmptyArray Number -> D3Selection -> Effect D3Selection
foreign import yAxis :: Coord (Endo Number) -> Number -> D3Selection -> Effect D3Selection
foreign import textDimensions :: String -> Dimensions Int
foreign import line :: Coord (Endo Number) -> Array (Coord Number) -> String
foreign import text :: String -> D3Selection -> Effect Unit
foreign import dimensions :: D3Selection -> Array (Dimensions Int)
foreign import selectAll :: D3Selection -> String -> D3Selection
