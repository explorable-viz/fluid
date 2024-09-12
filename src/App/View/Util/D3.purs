module App.View.Util.D3 where

import Prelude

import App.Util (Dimensions)
import Bind (Bind, (↦))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
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

translate :: Coord Int -> Bind String
translate { x, y } = "transform" ↦ "translate(" <> show x <> ", " <> show y <> ")"

translate' :: forall a. (a -> Coord Int) -> Bind (a -> String)
translate' f =
   "transform" ↦ \a -> let { x, y } = f a in "translate(" <> show x <> ", " <> show y <> ")"

rotate :: Int -> Bind String
rotate n = "transform" ↦ "rotate(" <> show n <> ")"

data SVGElementType =
   Circle | G | Path | Rect | SVG | Text

create :: SVGElementType -> D3Selection -> Array (Bind String) -> Effect D3Selection
create elementType parent = fromFoldable >>> createChild parent (show elementType)

createMany :: forall a. SVGElementType -> D3Selection -> String -> Array a -> Array (Bind (a -> String)) -> Effect D3Selection
createMany elementType parent class_ xs = fromFoldable >>> createChildren parent (show elementType) class_ xs

foreign import data D3Selection :: Type

foreign import createChild :: D3Selection -> String -> Object String -> Effect D3Selection
foreign import createChildren :: forall a. D3Selection -> String -> String -> Array a -> Object (a -> String) -> Effect D3Selection
foreign import remove :: D3Selection -> Effect Unit
foreign import nameCol :: String -> Array String -> String
foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number
-- Currently two different protocols for x and y axis -- will subsume into something more general
foreign import xAxis :: Coord (Endo Number) -> NonEmptyArray Number -> D3Selection -> Effect D3Selection
foreign import yAxis :: Coord (Endo Number) -> Number -> D3Selection -> Effect D3Selection
foreign import textDimensions :: String -> Dimensions Int
foreign import line :: Coord (Endo Number) -> Array (Coord Number) -> String
foreign import text :: String -> D3Selection -> Effect Unit
foreign import dimensions :: D3Selection -> Array (Dimensions Int)
foreign import selectAll :: D3Selection -> String -> D3Selection
foreign import attrs :: D3Selection -> Object String -> Effect D3Selection
foreign import styles :: D3Selection -> Object String -> Effect D3Selection

-- ======================
-- boilerplate
-- ======================

derive instance Generic SVGElementType _

instance Show SVGElementType
   where
   show = genericShow >>> toLower
