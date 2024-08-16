module App.View.Util where

import Prelude

import App.Util (SelState, Selectable, Selector, 𝕊, ViewSelector, selClasses, selClassesFor, selectionEventData)
import Data.Tuple (fst, snd, uncurry)
import Effect (Effect)
import Lattice ((∨))
import Val (Val)
import Web.Event.EventTarget (EventListener, eventListener)

type HTMLId = String
type Redraw = Selector Val -> Effect Unit

newtype View = View (forall r. (forall a. View' a => a -> r) -> r)

pack :: forall a. View' a => a -> View
pack x = View \k -> k x

unpack :: forall r. View -> (forall a. View' a => a -> r) -> r
unpack (View vw) k = vw k

selListener :: forall a. Redraw -> ViewSelector a -> Effect EventListener
selListener redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> redraw)

class Drawable a b | a -> b where
   draw :: Redraw -> RendererSpec a b -> Effect Unit

class View' a where
   drawView' :: HTMLId -> String -> Redraw -> a -> Effect Unit

drawView :: HTMLId -> String -> Redraw -> View -> Effect Unit
drawView divId suffix redraw vw =
   unpack vw (drawView' divId suffix redraw)

-- Heavily curried type isn't convenient for FFI
type RendererSpec a b =
   { divId :: HTMLId
   , suffix :: String
   , view :: a
   , viewState :: b
   }

type Renderer a b = UIHelpers -> RendererSpec a b -> EventListener -> Effect Unit

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
