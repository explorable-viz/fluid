module Link where

import Prelude hiding (join)

import App.Util (SelStates, Selectable, 𝕊, classes, selectionEventData')
import App.Util.Selector (ViewSelSetter)
import App.View.Util (class Drawable, Select, registerMouseListeners)
import App.View.Util.D3 (create, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import App.View.Util.Text (class Textual, textAttrs)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first)
import Data.Tuple (uncurry)
import DataType (cLink)
import Effect (Effect)
import Lattice (bot, join)
import Partial.Unsafe (unsafePartial)
import Util ((×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (eventListener)

data Link a = Link (Val a) (Selectable String)

linkContents :: ∀ a. Link a -> String
linkContents (Link _ (s × _)) = s

instance Drawable (Link (SelStates 𝕊)) where
   createRootElement _ = createRootElement
   setSelStates = setSelState

selLink :: ViewSelSetter (Link (SelStates 𝕊))
selLink _ δv = unsafePartial $ case _ of
   (Val α doc (Constr c (v1 : v2 : Nil))) | c == cLink ->
      first (\v1' -> Val α doc (Constr c (v1' : v2 : Nil))) (δv v1)

createRootElement :: Link (SelStates 𝕊) -> D3.Selection -> Effect D3.Selection
createRootElement link parent = do
   rootElement <- parent # create D3.Text [ classes [ "link" ] ]
   rootElement # setText (linkContents link) >>= setDatum link

-- Textual styling can be factored out into shared functionality
setSelState :: Link (SelStates 𝕊) -> Select -> D3.Selection -> Effect Unit
setSelState link redraw rootElement = do
   listener <- eventListener (redraw <<< uncurry selLink <<< selectionEventData')
   rootElement # setStyles (textAttrs link) >>= registerMouseListeners listener

instance Textual (Link (SelStates 𝕊)) where
   getText (Link v (s × _)) = s × (foldr join bot v)

