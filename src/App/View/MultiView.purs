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
   draw { divId, view: MultiView views } figVal redraw =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiViewEntry x >>> figVal) redraw) views

derive instance Newtype MultiView _
