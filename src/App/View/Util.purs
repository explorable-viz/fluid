module App.View.Util where

import Prelude
import App.Util (SelState, Selectable, 𝕊, Selector)
import Effect (Effect)
import Val (Val)
import Web.Event.EventTarget (EventListener)

type HTMLId = String
type Redraw = Selector Val -> Effect Unit

-- Heavily curried type isn't convenient for FFI
type RendererSpec a =
   { uiHelpers :: UIHelpers
   , divId :: HTMLId
   , suffix :: String
   , view :: a
   }

type Renderer a = RendererSpec a -> EventListener -> Effect Unit

-- Bundle into a record so we can export via FFI
type UIHelpers =
   { val :: forall a. Selectable a -> a
   , selState :: forall a. Selectable a -> SelState 𝕊
   , join :: SelState 𝕊 -> SelState 𝕊 -> SelState 𝕊
   , selClasses :: String
   , selClassesFor :: SelState 𝕊 -> String
   }
