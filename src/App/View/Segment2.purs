module App.View.Segment2 where

import Prelude

import App.Util (Attrs, Dimensions, 𝕊(..), Selectable, classes, colorShade, contents, getPersistent, getTransient, sel, selectionEventData')
import App.Util.Selector (jthSegment)
import App.View.Util (class View2, Select, registerMouseListeners)
import App.View.Util.D3 (ElementType(..), bandwidth, colorScale, create, setAttrs)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Int (toNumber)
import Data.Newtype (unwrap)
import Data.Tuple (uncurry)
import Effect (Effect)
import Util (Endo)
import Web.Event.EventTarget (eventListener)

newtype Segment = Segment
   { y :: Selectable String
   , z :: Selectable Number
   , j :: Int -- TODO: remove me (numerical index of my y coordinate)
   }

type Scales = { x :: String -> Number, y :: Endo Number }

type SegmentContext =
   { interior :: Dimensions Int
   , scales :: Scales
   , strokeWidth :: Int
   , x :: String
   , y :: Number
   , y_index :: Int
   }

instance View2 Segment SegmentContext where
   createRootElement2 = createRootElement'
   setSelStates2 = setSelStates'

createRootElement' :: SegmentContext -> Segment -> D3.Selection -> Effect D3.Selection
createRootElement' { interior, scales, strokeWidth, x, y } (Segment { z }) parent =
   parent
      # create Rect
           [ "x" ⟼ scales.x x
           , "y" ⟼ scales.y (contents z + y)
           , "height" ⟼ toNumber ((unwrap interior).height - strokeWidth) - scales.y (contents z)
           , "stroke-width" ⟼ strokeWidth
           , "width" ⟼ bandwidth scales.x
           , classes [ "bar" ]
           ]

setSelStates' :: SegmentContext -> Segment -> Select -> D3.Selection -> Effect Unit
setSelStates' { y_index } (Segment { z }) select segment = do
   listener <- eventListener (select <<< uncurry jthSegment <<< selectionEventData')
   segment # setAttrs attrs >>= registerMouseListeners listener
   where
   attrs :: Attrs
   attrs =
      [ "fill" ↦
           case persistent of
              None -> col'
              Secondary -> "url(#diagonalHatch-" <> show y_index <> ")"
              Primary -> colorShade col' (-40)
      , "stroke-width" ↦ "1"
      , "stroke-dasharray" ↦ case transient of
           None -> "none"
           Secondary -> "0.5 1" -- "1 2"
           Primary -> "0.5 1" -- "2 2"
      , "stroke-linecap" ↦ "round"
      , "stroke" ↦
           if persistent /= None || transient /= None then colorShade col' (-70)
           else col'
      ]
      where
      t = sel z
      persistent = getPersistent t
      transient = getTransient t
      col' = indexCol y_index

indexCol :: Int -> String
indexCol = colorScale "schemeAccent"
