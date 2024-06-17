module App.View.Util where

import Prelude

import App.Util (SelState, Selectable, Selector, ViewSelector, 𝕊, selClasses, selClassesFor, selectionEventData)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Lattice ((∨))
import Val (Val)
import Web.Event.EventTarget (EventListener, eventListener)

type HTMLId = String
type Redraw = Selector Val -> Effect Unit

selListener :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener redraw selector = eventListener (selectionEventData >>> uncurry selector >>> redraw)

class Drawable a b | a -> b where
   initialState :: a -> b
   draw :: HTMLId -> String -> Redraw -> a -> b -> Effect Unit

-- Heavily curried type isn't convenient for FFI
type RendererSpec a =
   { uiHelpers :: UIHelpers
   , divId :: HTMLId
   , suffix :: String
   , view :: a
   }

type Renderer a = RendererSpec a -> EventListener -> Effect Unit

type UIHelpers =
   { val :: forall a. Selectable a -> a
   , selState :: forall a. Selectable a -> SelState 𝕊
   , join :: SelState 𝕊 -> SelState 𝕊 -> SelState 𝕊
   , selClasses :: String
   , selClassesFor :: SelState 𝕊 -> String
   }

uiHelpers :: UIHelpers
uiHelpers =
   { val: fst
   , selState: snd
   , join: (∨)
   , selClasses
   , selClassesFor
   }
