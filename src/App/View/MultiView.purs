module App.View.MultiView where

import Prelude

import App.Util.Selector (multiPlotEntry)
import App.View.Util (class Drawable, class View', View, drawView)
import Data.Foldable (sequence_)
import Data.Newtype (class Newtype)
import Dict (Dict)
import Util.Map (mapWithKey)

newtype MultiView = MultiView (Dict View)

instance View' MultiView where
   drawView' divId _ redraw (MultiView vws) =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

instance Drawable MultiView Unit where
   draw redraw { divId, view: MultiView vws } =
      sequence_ $ mapWithKey (\x -> drawView divId x (multiPlotEntry x >>> redraw)) vws

derive instance Newtype MultiView _

