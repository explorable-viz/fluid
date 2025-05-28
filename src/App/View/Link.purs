module Link where

import Prelude hiding (join)

import App.Util (Attrs, SelStates, Selectable, 𝕊, isPersistent, isPrimary, isSecondary, isTransient, sel)
import App.View.Util (registerMouseListeners)
import App.View.Util.D3 (setStyles)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import DataType (cLink)
import Effect (Effect)
import Lattice (bot, join)
import Primitive (typeError)
import Util ((×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

data Link a = Link (Val a) (Selectable String)

getText :: Link (SelStates 𝕊) -> Selectable String
getText (Link v (s × _)) = s × (foldr join bot v)

linkContents :: ∀ a. Link a -> String
linkContents (Link _ (s × _)) = s

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
