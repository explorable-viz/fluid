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
   draw divId _ figView redraw (MultiView vws) =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiViewEntry x >>> figView) redraw) vws

derive instance Newtype MultiView _
