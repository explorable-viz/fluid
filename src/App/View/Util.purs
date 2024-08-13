module App.View.Util
   ( HTMLId
   , RRenderer
   , RRendererSpec
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

type RRendererSpec a b =
   { uiHelpers :: UIHelpers
   , divId :: HTMLId
   , suffix :: String
   , view :: a
   , viewState :: b
   }

type RRenderer a b = RRendererSpec a b -> EventListener -> Effect Unit
{-}
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
-}
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
   -- need to adapt the semilattice to accept inert as well
   -- at least, make a decision as to how much inert we want to present
   -- do we really, or do we just find another way through?

   , selClasses
   , selClassesFor
   }
