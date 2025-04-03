module App.View.Util where

import Prelude

import App.Util (SelState, SelStates, Selectable, Selection, SelectionType, SetSel, 𝕊, selClasses, selClassesFor, selectionEventData, selectionEventData')
import App.Util.Selector (ViewSelSetter, ViewSelSetter')
import App.View.Util.D3 (isEmpty, on, rootSelect, select)
import App.View.Util.D3 as D3
import Bind (Bind, Var)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Tuple (fst, snd, uncurry)
import Dict (Dict)
import Effect (Effect)
import Graph (DVertex, Vertex, Query)
import Lattice (𝔹, Raw, (∨))
import Module.Web (File, Folder)
import SExpr as S
import Util (type (×), Endo, Setter, check)
import Val (Env, Val)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, eventListener)

type HTMLId = String
type Redraw = Endo Fig -> Effect Unit

type Redraw' = SetSel Fig -> Effect Unit

newtype View = View (forall r. (forall a. Drawable a => a -> r) -> r)

pack :: forall a. Drawable a => a -> View
pack x = View \k -> k x

unpack :: forall r. View -> (forall a. Drawable a => a -> r) -> r
unpack (View vw) k = vw k

selListener :: forall a. Setter Fig (Val (SelStates 𝔹)) -> Redraw -> ViewSelSetter a -> Effect EventListener
selListener figVal redraw selector =
   eventListener (selectionEventData >>> uncurry selector >>> figVal >>> redraw)

selListener' :: forall a. (SetSel (Val (SelStates 𝔹)) -> Endo Fig) -> Redraw -> ViewSelSetter' a -> Effect EventListener
selListener' figVal redraw selector =
   eventListener (selectionEventData' >>> uncurry selector >>> figVal >>> redraw)

class Drawable a where
   draw :: RendererSpec a -> Setter Fig (Val (SelStates 𝔹)) -> Setter Fig View -> Redraw -> Effect Unit
   draw'' :: RendererSpec a -> (SetSel (Val (SelStates 𝔹)) -> Endo Fig) -> Setter Fig View -> Redraw -> Effect Unit

-- Merge into Drawable once JS->PS transition complete
class Drawable2 a where
   createRootElement :: a -> D3.Selection -> String -> Effect D3.Selection
   setSelStates :: a -> EventListener -> D3.Selection -> Effect Unit

draw' :: forall a. Drawable2 a => Renderer a
draw' _ { divId, suffix, view } redraw = do
   let childId = divId <> "-" <> suffix
   div <- rootSelect ("#" <> divId)
   isEmpty div <#> not >>= flip check ("Unable to insert figure: no div found with id " <> divId)
   maybeRootElement <- div # select ("#" <> childId)
   setSelStates view redraw =<<
      ( isEmpty maybeRootElement >>=
           if _ then createRootElement view div childId
           else pure maybeRootElement
      )

drawView' :: RendererSpec View -> (SetSel (Val (SelStates 𝔹)) -> Endo Fig) -> Setter Fig View -> Redraw -> Effect Unit
drawView' rSpec@{ view: vw } figVal figView redraw =
   unpack vw (\view -> draw'' (rSpec { view = view }) figVal figView redraw)

drawView :: RendererSpec View -> Setter Fig (Val (SelStates 𝔹)) -> Setter Fig View -> Redraw -> Effect Unit
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
   , selState :: forall a. Selectable a -> SelStates 𝕊
   , join :: SelStates 𝕊 -> SelStates 𝕊 -> SelStates 𝕊
   , selClasses :: String
   , selClassesFor :: SelStates 𝕊 -> String
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
   { fluidSrcPaths :: Array Folder
   , imports :: Array String
   , datasets :: Array (Bind String)
   , file :: File
   , inputs :: Array Var
   , query :: Maybe (Query (Val Vertex))
   }

data Direction = LinkedInputs | LinkedOutputs | Intermediates

type Fig =
   { spec :: FigSpec
   , s :: Raw S.Expr
   , γ :: Env (SelStates 𝔹)
   , v :: Val (SelStates 𝔹)
   , ι :: Env (SelStates 𝔹)
   , ια :: Env Vertex
   , dir :: Selection Direction
   , linkedInputs :: SelectionType -> Env (SelStates 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
   , linkedOutputs :: SelectionType -> Val (SelStates 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
   , linkIntermediates :: Env (SelStates 𝔹) -> Env Vertex -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
   , in_views :: Dict (Maybe View) -- strengthen this
   , in_roots :: Set Vertex
   , out_view :: Maybe View
   , intermediate_views :: Dict (Maybe View)
   , inerts :: Set DVertex
   }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
