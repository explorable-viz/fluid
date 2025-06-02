module App.View.Util.Text where

import Prelude

import App.Util (Attrs, Selectable, isPersistent, isPrimary, isSecondary, isTransient, sel)
import Bind ((↦))

class Textual a where
   getText :: a -> Selectable String

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
