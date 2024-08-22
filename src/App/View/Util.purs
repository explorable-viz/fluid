module App.View.Util where

import Prelude

import App.Util (SelState, Selectable, 𝕊, Sel, selClasses, selClassesFor, selectionEventData)
import App.Util.SelSetter (ViewSelSetter)
import Bind (Bind, Var)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd, uncurry)
import Dict (Dict)
import Effect (Effect)
import GaloisConnection (GaloisConnection)
import Lattice (𝔹, Raw, (∨))
import Module (File)
import SExpr as S
import Unsafe.Coerce (unsafeCoerce)
import Util (Endo, Setter)
import Val (Env, Val)
import Web.Event.EventTarget (EventListener, eventListener)

type HTMLId = String
type Redraw = Endo Fig -> Effect Unit

newtype View = View (forall r. (forall a. Drawable a => a -> r) -> r)
newtype ViewState = ViewState (forall r. (forall a b. Drawable2 a b => b -> r) -> r)

pack :: forall a. Drawable a => a -> View
pack x = View \k -> k x

pack2 :: forall a b. Drawable2 a b => b -> ViewState
pack2 x = ViewState \k -> k x

unpack :: forall r. View -> (forall a. Drawable a => a -> r) -> r
unpack (View vw) k = vw k

unpack2 :: forall r. ViewState -> (forall a b. Drawable2 a b => b -> r) -> r
unpack2 (ViewState vw) k = vw k

unsafeUnpack :: forall a. Drawable a => View -> a
unsafeUnpack vw = unpack vw (unsafeCoerce (\x -> x))

unsafeView :: forall a. Drawable a => Setter View a
unsafeView δvw vw = pack (δvw (unsafeUnpack vw))

selListener :: forall a. Setter Fig (Sel Val) -> Redraw -> ViewSelSetter a -> Effect EventListener
selListener figVal redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> figVal >>> redraw)

class Drawable a where
   draw :: RendererSpec a -> Setter Fig (Sel Val) -> Setter Fig View -> Redraw -> Effect Unit

class Drawable2 a b | b -> a where
   draw2 :: RendererSpec2 a b -> Setter Fig (Sel Val) -> Setter Fig View -> Redraw -> Effect Unit

drawView :: RendererSpec View -> Setter Fig (Sel Val) -> Setter Fig View -> Redraw -> Effect Unit
drawView rSpec@{ view: vw } figVal figView redraw =
   unpack vw (\view -> draw (rSpec { view = view }) figVal figView redraw)

-- Heavily curried type isn't convenient for FFI
type RendererSpec a =
   { divId :: HTMLId
   , suffix :: String
   , view :: a
   }

type RendererSpec2 a b =
   { divId :: HTMLId
   , suffix :: String
   , view :: a
   , viewState :: b
   }

type Renderer a = UIHelpers -> RendererSpec a -> EventListener -> Effect Unit

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
   , γ :: Sel Env
   , v :: Sel Val
   , gc :: GaloisConnection (Env 𝔹) (Val 𝔹)
   , gc_dual :: GaloisConnection (Val 𝔹) (Env 𝔹)
   , dir :: Direction
   , in_views :: Dict (Maybe View)
   , out_view :: Maybe View
   }

type Fig2 =
   { spec :: FigSpec
   , s :: Raw S.Expr
   , γ :: Sel Env
   , v :: Sel Val
   , gc :: GaloisConnection (Env 𝔹) (Val 𝔹)
   , gc_dual :: GaloisConnection (Val 𝔹) (Env 𝔹)
   , dir :: Direction
   , in_viewStates :: Dict (Maybe ViewState)
   , out_viewState :: Maybe ViewState
   }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
