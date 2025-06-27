module Link where

import Prelude hiding (join)

import App.Util (SelStates, Selectable, 𝕊, classes, selectionEventData')
import App.Util.Selector (ViewSelSetter)
import App.View.Text (class Textual, textAttrs)
import App.View.Util (class View, class View2, Select, registerMouseListeners)
import App.View.Util.D3 (create, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
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

data Link = Link (Val (SelStates 𝕊)) (Selectable String)

linkContents :: Link -> String
linkContents (Link _ (s × _)) = s

instance View Link Unit Unit where
   createRootElement _ = createRootElement
   setSelStates = setSelState

instance View2 Link Unit where
   createElement _ = createRootElement
   setSelection _ = setSelState

selLink :: ViewSelSetter Link
selLink _ δv = unsafePartial $ case _ of
   (Val α doc (Constr c (v1 : v2 : Nil))) | c == cLink ->
      first (\v1' -> Val α doc (Constr c (v1' : v2 : Nil))) (δv v1)

createRootElement :: Link -> D3.Selection -> Effect D3.Selection
createRootElement link parent = do
   rootElement <- parent # create D3.Text [ classes [ "link" ] ]
   rootElement # setText (linkContents link) >>= setDatum link

-- Textual styling can be factored out into shared functionality
setSelState :: Link -> Select -> D3.Selection -> Effect Unit
setSelState link redraw rootElement = do
   listener <- eventListener (redraw <<< uncurry selLink <<< selectionEventData')
   rootElement # setStyles (textAttrs link) >>= registerMouseListeners listener

instance Textual Link where
   getText (Link v (s × _)) = s × foldr join bot v
