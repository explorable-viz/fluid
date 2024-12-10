module App.View.Util where

import Prelude

import App.Util (SelState, Selectable, 𝕊, selClasses, selClassesFor, selectionEventData)
import App.Util.Selector (ViewSelSetter)
import App.View.Util.D3 (isEmpty, on, rootSelect, select)
import App.View.Util.D3 as D3
import Bind (Bind, Var)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd, uncurry)
import Dict (Dict)
import Effect (Effect)
import GaloisConnection (GaloisConnection)
import Lattice (𝔹, Raw, (∨))
import Module.Files (File)
import SExpr as S
import Util (type (×), Endo, Setter, check)
import Val (Env, Val)
import Web.Event.Event (EventType(..))
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

class Drawable a where
   draw :: RendererSpec a -> Setter Fig (Val (SelState 𝔹)) -> Setter Fig View -> Redraw -> Effect Unit

-- Merge into Drawable once JS->PS transition complete
class Drawable2 a where
   createRootElement :: a -> D3.Selection -> String -> Effect D3.Selection
   setSelState :: a -> EventListener -> D3.Selection -> Effect Unit

draw' :: forall a. Drawable2 a => Renderer a
draw' _ { divId, suffix, view } redraw = do
   let childId = divId <> "-" <> suffix
   div <- rootSelect ("#" <> divId)
   isEmpty div <#> not >>= flip check ("Unable to insert figure: no div found with id " <> divId)
   maybeRootElement <- div # select ("#" <> childId)
   setSelState view redraw =<<
      ( isEmpty maybeRootElement >>=
           if _ then createRootElement view div childId
           else pure maybeRootElement
      )

drawView :: RendererSpec View -> Setter Fig (Val (SelState 𝔹)) -> Setter Fig View -> Redraw -> Effect Unit
drawView rSpec@{ view: vw } figVal figView redraw =
   unpack vw (\view -> draw (rSpec { view = view }) figVal figView redraw)

registerMouseListeners :: EventListener -> D3.Selection -> Effect Unit
registerMouseListeners redraw element = do
   for_ [ "mousedown", "mouseenter", "mouseleave" ] \ev ->
      element # on (EventType ev) redraw

-- Heavily curried type isn't convenient for FFI
type RendererSpec a =
   { divId :: HTMLId
   , suffix :: String
   , view :: a
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
   , γ :: Env (SelState 𝔹)
   , v :: Val (SelState 𝔹)
   , linkedOutputs :: GaloisConnection (Val (SelState 𝔹) × Env (SelState 𝔹)) (Val (SelState 𝔹))
   , linkedInputs :: GaloisConnection (Env (SelState 𝔹) × Val (SelState 𝔹)) (Env (SelState 𝔹))
   , dir :: Direction
   , in_views :: Dict (Maybe View) -- strengthen this
   , out_view :: Maybe View
   }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
