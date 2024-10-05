module App.View.Util where

import Prelude

import App.Util (Dimensions(..), SelState, Selectable, 𝕊, classes, selClasses, selClassesFor, selectionEventData)
import App.Util.Selector (ViewSelSetter)
import App.View.Util.Axes (Orientation(..))
import App.View.Util.D3 (Coord, ElementType(..), create, isEmpty, on, rootSelect, rotate, scaleLinear, select, selectAll, setAttrs, setStyles, translate, xAxis, yAxis)
import App.View.Util.D3 as D3
import App.View.Util.Point (Point(..))
import Bind (Bind, Var, (↦))
import Data.Array.NonEmpty (NonEmptyArray, nub)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Tuple (fst, snd, uncurry)
import Dict (Dict)
import Effect (Effect)
import GaloisConnection (GaloisConnection)
import Lattice (𝔹, Raw, (∨))
import Module (File)
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

class HasAxes a where
   -- all data elements to be plotted, for computing axis mappings
   points :: a -> Coord (NonEmptyArray Number)
   tickLabels :: a -> Point Orientation

to :: forall a. HasAxes a => a -> Dimensions Int -> Coord (Endo Number)
to view (Dimensions range) =
   { x: scaleLinear { min: min'.x, max: max'.x } { min: 0.0, max: toNumber range.width }
   , y: scaleLinear { min: 0.0, max: max'.y } { min: toNumber range.height, max: 0.0 }
   }
   where
   { x: xs, y: ys } = points view

   min' :: Coord Number
   min' = { x: minimum xs, y: minimum ys }

   max' :: Coord Number
   max' = { x: maximum xs, y: maximum ys }

createAxes :: forall a. HasAxes a => a -> Dimensions Int -> D3.Selection -> Effect (Coord D3.Selection)
createAxes view range parent = do
   let Point { x: xLabels, y: yLabels } = tickLabels view
   x <- xAxis (to' range) (nub points'.x) =<<
      (parent # create G [ classes [ "x-axis" ], translate { x: 0, y: (unwrap range).height } ])
   when (fst xLabels == Rotated) do
      labels <- x # selectAll "text"
      for_ labels $
         setAttrs [ rotate 45 ] >=> setStyles [ "text-anchor" ↦ "start" ]
   y <- yAxis (to' range) 3.0 =<< (parent # create G [ classes [ "y-axis" ] ])
   when (fst yLabels == Rotated) do
      labels <- y # selectAll "text"
      for_ labels $
         setAttrs [ rotate 45 ] >=> setStyles [ "text-anchor" ↦ "end" ]
   pure { x, y }
   where
   to' = to view
   points' = points view

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
