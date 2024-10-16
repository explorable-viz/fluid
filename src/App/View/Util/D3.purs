module App.View.Util.D3
   ( Coord
   , Selection
   , Margin
   , ElementType(..)
   , attrs
   , classed
   , colorScale
   , create
   , datum
   , dimensions
   , isEmpty
   , line
   , on
   , remove
   , rootSelect
   , rotate
   , rotate'
   , scaleLinear
   , select
   , selectAll
   , setAttrs
   , setDatum
   , setStyles
   , setText
   , textHeight
   , textWidth
   , translate
   , xAxis
   , yAxis
   ) where

import Prelude

import App.Util (Dimensions, Attrs)
import Bind (Bind, (↦))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Effect (Effect)
import Foreign.Object (Object, fromFoldable)
import Util (Endo)
import Web.Event.Event (EventType)
import Web.Event.EventTarget (EventListener)

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

textWidth :: String -> String -> Int
textWidth class_ = textDimensions class_ >>> unwrap >>> _.width

textHeight :: String -> String -> Int
textHeight class_ = textDimensions class_ >>> unwrap >>> _.height

translate :: Coord Int -> Bind String
translate { x, y } = "transform" ↦ "translate(" <> show x <> ", " <> show y <> ")"

rotate :: Int -> Bind String
rotate n = "transform" ↦ "rotate(" <> show n <> ")"

rotate' :: forall a. (a -> Int) -> Bind (a -> String)
rotate' f = "transform" ↦ \a -> "rotate(" <> show (f a) <> ")"

-- Might be some PureScript library that could help here
data ElementType
   = Caption
   | Circle
   | G
   | Path
   | Rect
   | SVG
   | Table
   | Text
   | TBody
   | TD
   | TH
   | THead
   | TR

create :: ElementType -> Attrs -> Selection -> Effect Selection
create elementType as parent =
   fromFoldable as # createChild parent (show elementType)

setAttrs :: Attrs -> Selection -> Effect Selection
setAttrs as sel = fromFoldable as # attrs sel

setStyles :: Attrs -> Selection -> Effect Selection
setStyles as sel = fromFoldable as # styles sel

-- Could feasibly rename to Element
foreign import data Selection :: Type

foreign import createChild :: Selection -> String -> Object String -> Effect Selection
foreign import remove :: Selection -> Effect Unit
foreign import colorScale :: String -> String -> Array String -> String
foreign import scaleLinear :: { min :: Number, max :: Number } -> { min :: Number, max :: Number } -> Endo Number
-- Currently two different protocols for x and y axis; will subsume into something more general
foreign import xAxis :: Coord (Endo Number) -> NonEmptyArray Number -> Selection -> Effect Selection
foreign import yAxis :: Coord (Endo Number) -> Number -> Selection -> Effect Selection
foreign import isEmpty :: Selection -> Effect Boolean
foreign import dimensions :: Selection -> Effect (Dimensions Int)
foreign import textDimensions :: String -> String -> Dimensions Int
foreign import line :: Coord (Endo Number) -> Array (Coord Number) -> String
foreign import rootSelect :: String -> Effect Selection
foreign import select :: String -> Selection -> Effect Selection
foreign import selectAll :: String -> Selection -> Effect (Array Selection)
foreign import setText :: String -> Selection -> Effect Selection
foreign import attrs :: Selection -> Object String -> Effect Selection
foreign import styles :: Selection -> Object String -> Effect Selection
foreign import classed :: String -> Boolean -> Selection -> Effect Selection
foreign import setDatum :: forall a. a -> Selection -> Effect Selection
foreign import datum :: forall a. Selection -> Effect a
foreign import on :: EventType -> EventListener -> Selection -> Effect Selection

instance Show ElementType
   where
   show = genericShow >>> toLower

-- ======================
-- boilerplate
-- ======================

derive instance Generic ElementType _
