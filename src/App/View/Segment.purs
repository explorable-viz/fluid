module App.Segment where

import Prelude

import App.Util (Attrs, PartialAttrs, Selectable, 𝕊(..), classes, colorShade, contents, getPersistent, getTransient, sel, selectionEventData')
import App.Util.Selector (jthSegment)
import App.View.Util (class View, Select, registerMouseListeners)
import App.View.Util.D3 (ElementType(..), colorScale, create, datum, setAttrs, setDatum)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Web.Event.EventTarget (eventListener)

newtype Segment = Segment
   { y :: Selectable String
   , z :: Selectable Number
   , j :: Int -- TODO: remove me
   }

createRootElement' :: PartialAttrs { y :: String, z :: Number } Unit -> Segment -> D3.Selection -> Effect D3.Selection
createRootElement' attrFun (Segment { y, z, j }) parent =
   parent
      # create Rect (attrFun { y: contents y, z: contents z } [ classes [ "bar" ] ] unit)
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

instance View Segment { y :: String, z :: Number } Unit where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

indexCol :: Int -> String
indexCol = colorScale "schemeAccent"
