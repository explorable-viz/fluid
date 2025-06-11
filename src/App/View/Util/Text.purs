module App.View.Util.Text where

import Prelude

import App.Util (Attrs, Selectable, inert, isPersistent, isPrimary, isSecondary, isTransient, sel, selectionEventData')
import App.Util.Selector (ViewSelSetter)
import App.View.Util (class Drawable, Select, registerMouseListeners)
import App.View.Util.D3 (create, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Tuple (uncurry)
import Effect (Effect)
import Util ((×))
import Web.Event.EventTarget (eventListener)

class Textual a where
   getText :: a -> Selectable String

newtype Text = Text (Selectable String)

createRootElement :: Text -> D3.Selection -> Effect D3.Selection
createRootElement (Text (text × _)) parent = do
   rootElement <- parent # create D3.Text [ "class" ↦ "para-text" ]
   rootElement # setText text

setSelStates :: Text -> Select -> D3.Selection -> Effect Unit
setSelStates (Text (text × _)) redraw rootElement = do
   elem <- rootElement # D3.select ".para-text"
   listener <- eventListener (redraw <<< uncurry textSelector <<< selectionEventData')
   elem # setStyles (textAttrs text) >>= registerMouseListeners listener
   where
   textSelector :: ViewSelSetter Text
   textSelector _ = identity

instance Drawable Text where
   createRootElement = createRootElement
   setSelStates = setSelStates

instance Textual String where
   getText x = x × inert

textAttrs :: ∀ a. Textual a => a -> Attrs
textAttrs x =
   [ "border-bottom" ↦ border
   , "background" ↦ background
   , "color" ↦ color
   ]
   where
   text = getText x
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

type TextElem = { i :: Int }
