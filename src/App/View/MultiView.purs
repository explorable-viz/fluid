module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class Drawable, View, drawView)
import Data.Foldable (sequence_)
import Data.Newtype (class Newtype)
import Dict (Dict)
import Util.Map (mapWithKey)

newtype MultiView = MultiView (Dict View)

instance Drawable MultiView where
   draw { divId, view: MultiView views } figVal figView redraw =
      sequence_ $ flip mapWithKey views \x view ->
         drawView { divId, suffix: x, view } (multiViewEntry x >>> figVal) figView redraw

derive instance Newtype MultiView _
