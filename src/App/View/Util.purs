module App.View.Util
   ( HTMLId
   , Renderer
   , RendererSpec
   , Redraw
   , UIHelpers
   , class Drawable
   , draw
   , selListener
   , uiHelpers
   ) where

import Prelude

import App.Util (ReactState, Selectable, selClasses, selClassesFor, joinR, Selector, ViewSelector, 𝕊, selectionEventData)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Val (Val)
import Web.Event.EventTarget (EventListener, eventListener)

type HTMLId = String
type Redraw = Selector Val -> Effect Unit

selListener :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> redraw)

--need to make Drawable a ReactState thing.
class Drawable a b | a -> b where
   draw :: HTMLId -> String -> Redraw -> a -> b -> Effect Unit

-- Heavily curried type isn't convenient for FFI

type RendererSpec a b =
   { uiHelpers :: UIHelpers
   , divId :: HTMLId
   , suffix :: String
   , view :: a
   , viewState :: b
   }

type Renderer a b = RendererSpec a b -> EventListener -> Effect Unit

type UIHelpers =
   { val :: forall a. Selectable a -> a
   , selState :: forall a. Selectable a -> ReactState 𝕊
   , join :: ReactState 𝕊 -> ReactState 𝕊 -> ReactState 𝕊
   , selClasses :: String
   , selClassesFor :: ReactState 𝕊 -> String
   }

uiHelpers :: UIHelpers
uiHelpers =
   { val: fst
   , selState: snd
   , join: joinR
   , selClasses
   , selClassesFor
   }
