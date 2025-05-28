module Link where

import Prelude hiding (join)

import App.Util (Attrs, SelStates, Selectable, 𝕊, classes, isPersistent, isPrimary, isSecondary, isTransient, sel)
import App.Util.Selector (ViewSelSetter, SelSetter)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (create, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first)
import DataType (cLink)
import Effect (Effect)
import Lattice (bot, join)
import Partial.Unsafe (unsafePartial)
import Primitive (typeError)
import Util ((×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

data Link a = Link (Val a) (Selectable String)

getText :: Link (SelStates 𝕊) -> Selectable String
getText (Link v (s × _)) = s × (foldr join bot v)

linkContents :: ∀ a. Link a -> String
linkContents (Link _ (s × _)) = s

instance Drawable (Link (SelStates 𝕊)) where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec =<< selListener figVal redraw selLink

instance Drawable2 (Link (SelStates 𝕊)) where
   createRootElement = createRootElement
   setSelStates = setSelState

selLink :: ViewSelSetter (Link (SelStates 𝕊))
selLink _ = fragment
   where
   fragment :: SelSetter Val Val
   fragment δv = unsafePartial $ case _ of
      (Val α doc (Constr c (v1 : v2 : Nil))) | c == cLink ->
         first (\v1' -> Val α doc (Constr c (v1' : v2 : Nil))) (δv v1)

createRootElement :: Link (SelStates 𝕊) -> D3.Selection -> String -> Effect D3.Selection
createRootElement link div childId = do
   rootElement <- div # create D3.Text [ classes [ "paragraph" ], "id" ↦ childId ]
   mkElem rootElement link
   where
   mkElem :: D3.Selection -> Link (SelStates 𝕊) -> Effect D3.Selection
   mkElem root link' = do
      elem <- root # create D3.Text [ classes [ "link" ] ]
      elem # setText (linkContents link') >>= setDatum link'

setSelState :: Link (SelStates 𝕊) -> EventListener -> D3.Selection -> Effect Unit
setSelState link redraw rootElement = do
   elem <- rootElement # D3.select ".link"
   elem # setStyles textAttrs >>= registerMouseListeners redraw
   where
   textAttrs :: Attrs
   textAttrs =
      [ "border-bottom" ↦ border
      , "background" ↦ background
      , "color" ↦ color
      ]
      where
      text = getText link
      sel' = sel text

      border :: String
      border
         | isTransient sel' = "1px solid blue"
         | otherwise = "none"

      background :: String
      background
         | isPrimary sel' && isPersistent sel' = "#93E9BE"
         | isSecondary sel' && isPersistent sel' = "rgb(226, 226, 226)"
         | otherwise = "white"

      color :: String
      color
         | isPrimary sel' && isTransient sel' = "blue"
         | isSecondary sel' && isTransient sel' = "royalblue"
         | otherwise = "black"

unpackLink :: BaseVal (SelStates 𝕊) -> Link (SelStates 𝕊)
unpackLink (Constr c (Val α doc v : (Val α' _ (Str s) : Nil))) | c == cLink = Link (Val α doc v) (s × α')
unpackLink v = typeError v "Link"
