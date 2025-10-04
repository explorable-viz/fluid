module App.View.MultiView where

import Prelude

import App.Util.Selector (multiView)
import App.View.Util (class Viewable, Select, View, createElement, setSelection)
import App.View.Util.D3 as D3
import Dict (Dict)
import Effect (Effect)
import Util (type (×))

data MultiView = MultiView (Dict (View × View))

instance Viewable MultiView Unit where
   createElement :: Unit -> MultiView -> D3.Selection -> Effect D3.Selection
   createElement _ (MultiView views) = createElement unit views

   setSelection :: Unit -> MultiView -> Select -> D3.Selection -> Effect Unit
   setSelection _ (MultiView views) select = setSelection unit views (multiView >>> select)
