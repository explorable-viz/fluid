module App.View.Util where

import Prelude

import App.Util (ReactState, SelState, Selectable, 𝕊, selClasses, selClassesFor, selectionEventData)
import App.Util.Selector (ViewSelSetter)
import Bind (Bind, Var)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd, uncurry)
import Dict (Dict)
import Effect (Effect)
import GaloisConnection (GaloisConnection)
import Lattice (𝔹, Raw, (∨))
import Module (File)
import SExpr as S
import Util (Endo, Setter)
import Val (Env, Val)
import Web.Event.EventTarget (EventListener, eventListener)

type HTMLId = String
type Redraw = Endo Fig -> Effect Unit

newtype View = View (forall r. (forall a. Drawable a => a -> r) -> r)

pack :: forall a. Drawable a => a -> View
pack x = View \k -> k x

unpack :: forall r. View -> (forall a. Drawable a => a -> r) -> r
unpack (View vw) k = vw k

selListener :: forall a. Setter Fig (Val (SelState 𝔹)) -> Redraw -> ViewSelSetter a -> Effect EventListener
selListener figVal redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> figVal >>> redraw)

--need to make Drawable a ReactState thing.
class Drawable a where
   draw :: RendererSpec a -> Setter Fig (Val (SelState 𝔹)) -> Setter Fig View -> Redraw -> Effect Unit

drawView :: RendererSpec View -> Setter Fig (Val (SelState 𝔹)) -> Setter Fig View -> Redraw -> Effect Unit
drawView rSpec@{ view: vw } figVal figView redraw =
   unpack vw (\view -> draw (rSpec { view = view }) figVal figView redraw)

-- Heavily curried type isn't convenient for FFI
type RendererSpec a =
   { divId :: HTMLId
   , suffix :: String
   , view :: a
   }

type Renderer a = UIHelpers -> RendererSpec a -> EventListener -> Effect Unit

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
   , γ0 :: Env 𝔹
   , v0 :: Val 𝔹
   }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
