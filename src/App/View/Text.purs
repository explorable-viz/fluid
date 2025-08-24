module App.View.Text where

import Prelude

import App.Util (Attrs, Selectable, isPersistent, isPrimary, isSecondary, isTransient, sel, selectionEventData')
import App.Util.Selector (ViewSelSetter)
import App.View.Util (class View, Select, registerMouseListeners)
import App.View.Util.D3 (create, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (uncurry)
import Effect (Effect)
import Util ((×))
import Web.Event.EventTarget (eventListener)

class Textual a where
   getText :: a -> Selectable String

newtype Text = Text (Selectable String)

instance View Text Unit where
   createElement :: Unit -> Text -> D3.Selection -> Effect D3.Selection
   createElement _ (Text (text × _)) parent = do
      rootElement <- parent # create D3.Span []
      rootElement # setText text

   setSelection :: Unit -> Text -> Select -> D3.Selection -> Effect Unit
   setSelection _ text redraw rootElement = do
      listener <- eventListener (redraw <<< uncurry textSelector <<< selectionEventData')
      rootElement # setStyles (textAttrs text) >>= registerMouseListeners listener
      where
      textSelector :: ViewSelSetter Text
      textSelector _ = identity

instance Textual Text where
   getText = unwrap

textAttrs :: ∀ a. Textual a => a -> Attrs
textAttrs text =
   [ "border-bottom" ↦ border
   , "background" ↦ background
   , "color" ↦ color
   ]
   where
   sel' = sel (getText text)

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

-- ======================
-- boilerplate
-- ======================

derive instance Newtype Text _
