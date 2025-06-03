module Link where

import Prelude hiding (join)

import App.Util (SelStates, Selectable, 𝕊, classes)
import App.Util.Selector (ViewSelSetter, SelSetter)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (create, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import App.View.Util.Text (class Textual, textAttrs)
import Bind ((↦))
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first)
import DataType (cLink)
import Effect (Effect)
import Lattice (bot, join)
import Partial.Unsafe (unsafePartial)
import Util ((×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

data Link a = Link (Val a) (Selectable String)

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

-- Textual styling can be factored out into shared functionality
setSelState :: Link (SelStates 𝕊) -> EventListener -> D3.Selection -> Effect Unit
setSelState link redraw rootElement = do
   rootElement # setStyles (textAttrs link) >>= registerMouseListeners redraw

instance Textual (Link (SelStates 𝕊)) where
   getText (Link v (s × _)) = s × (foldr join bot v)

