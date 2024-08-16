module App.View.Util where

import Prelude

import App.Util (SelState, Selectable, Selector, 𝕊, ViewSelector, selClasses, selClassesFor, selectionEventData)
import Bind (Bind, Var)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd, uncurry)
import Dict (Dict)
import Effect (Effect)
import GaloisConnection (GaloisConnection)
import Lattice (𝔹, Raw, (∨))
import Module (File)
import SExpr as S
import Util (Endo)
import Val (Env, Val)
import Web.Event.EventTarget (EventListener, eventListener)

type HTMLId = String
type Redraw = Endo Fig -> Effect Unit

newtype View = View (forall r. (forall a. Drawable a => a -> r) -> r)

pack :: forall a. Drawable a => a -> View
pack x = View \k -> k x

unpack :: forall r. View -> (forall a. Drawable a => a -> r) -> r
unpack (View vw) k = vw k

selListener :: forall a. (Selector Val -> Endo Fig) -> Redraw -> ViewSelector a -> Effect EventListener
selListener figView redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> figView >>> redraw)

class Drawable a where
   draw :: HTMLId -> String -> (Selector Val -> Endo Fig) -> Redraw -> a -> Effect Unit

drawView :: HTMLId -> String -> (Selector Val -> Endo Fig) -> Redraw -> View -> Effect Unit
drawView divId suffix figView redraw vw =
   unpack vw (draw divId suffix figView redraw)

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

type FigSpec =
   { imports :: Array String
   , datasets :: Array (Bind String)
   , file :: File
   , inputs :: Array Var
   }

data Direction = LinkedInputs | LinkedOutputs

type Fig =
   { spec :: FigSpec
   , s :: Raw S.Expr
   , γ :: Env (SelState 𝔹)
   , v :: Val (SelState 𝔹)
   , gc :: GaloisConnection (Env 𝔹) (Val 𝔹)
   , gc_dual :: GaloisConnection (Val 𝔹) (Env 𝔹)
   , dir :: Direction
   , in_views :: Dict (Maybe View) -- strengthen this
   , out_view :: Maybe View
   }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
