module App.View.Segment where

import Prelude

import App.Util (Attrs, Dimensions, PartialAttrs, Selectable, 𝕊(..), classes, colorShade, contents, getPersistent, getTransient, sel, selectionEventData')
import App.Util.Selector (jthSegment)
import App.View.Util (class View, class View2, Select, registerMouseListeners)
import App.View.Util.D3 (ElementType(..), bandwidth, colorScale, create, datum, setAttrs, setDatum)
import App.View.Util.D3 as D3
import Bind ((↦), (⟼))
import Data.Int (toNumber)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (uncurry)
import Effect (Effect)
import Util (Endo)
import Web.Event.EventTarget (eventListener)

newtype Segment = Segment
   { y :: Selectable String
   , z :: Selectable Number
   , j :: Int -- TODO: remove me (numerical index of my y coordinate)
   }

createRootElement' :: PartialAttrs { z :: Number } Unit -> Segment -> D3.Selection -> Effect D3.Selection
createRootElement' attrFun (Segment { z, j }) parent =
   parent
      # create Rect (attrFun { z: contents z } [ classes [ "bar" ] ] unit)
      >>= setDatum j

setSelStates' :: Segment -> Select -> D3.Selection -> Effect Unit
setSelStates' (Segment { z }) select segment = do
   listener <- eventListener (select <<< uncurry jthSegment <<< selectionEventData')
   j <- datum segment
   segment # setAttrs (barAttrs j) >>= registerMouseListeners listener
   where
   barAttrs :: Int -> Attrs
   barAttrs j =
      [ "fill" ↦
           case persistent of
              None -> col'
              Secondary -> "url(#diagonalHatch-" <> show j <> ")"
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
      col' = indexCol j

instance View Segment { z :: Number } Unit where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

type Scales = { x :: String -> Number, y :: Endo Number }

type SegmentContext =
   { interior :: Dimensions Int
   , scales :: Scales
   , strokeWidth :: Int
   , x :: String
   , y :: Number -- accumulated z values of segments below me in stack
   , y_index :: Int
   }

instance View2 Segment SegmentContext where
   createElement :: SegmentContext -> Segment -> D3.Selection -> Effect D3.Selection
   createElement { interior, scales, strokeWidth, x, y } (Segment { z }) parent =
      parent
         # create Rect
              [ "x" ⟼ scales.x x
              , "y" ⟼ scales.y (contents z + y)
              , "height" ⟼ toNumber ((unwrap interior).height - strokeWidth) - scales.y (contents z)
              , "stroke-width" ⟼ strokeWidth
              , "width" ⟼ bandwidth scales.x
              , classes [ "bar" ]
              ]

   setSelection :: SegmentContext -> Segment -> Select -> D3.Selection -> Effect Unit
   setSelection { y_index } (Segment { z }) select segment = do
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

-- ======================
-- boilerplate
-- ======================

derive instance Newtype Segment _
