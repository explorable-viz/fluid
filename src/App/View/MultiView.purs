module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class Drawable, class Drawable2, View, Select, createRootElement, draw', selListener', setSelStates, uiHelpers, unpack)
import App.View.Util.D3 (ElementType(..), create)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (sequence_)
import Data.Newtype (class Newtype)
import Dict (Dict)
import Effect (Effect)
import Util.Map (mapWithKey)

newtype MultiView = MultiView (Dict View)

instance Drawable MultiView where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec (selListener' figVal redraw)

instance Drawable2 MultiView where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: MultiView -> D3.Selection -> String -> Effect D3.Selection
createRootElement' (MultiView views) div childId = do
   rootElement <- div # create G [ "id" ↦ childId ]
   sequence_ $ flip mapWithKey views \x view -> do
      unpack view \v -> createRootElement v rootElement (childId <> "-" <> x)
   pure rootElement

setSelStates' :: MultiView -> Select -> D3.Selection -> Effect Unit
setSelStates' (MultiView views) redraw rootElement = do
   sequence_ $ flip mapWithKey views \x view -> do
      unpack view \v -> setSelStates v (redraw <<< multiViewEntry x) rootElement

derive instance Newtype MultiView _
