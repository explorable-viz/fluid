module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class Drawable, class Drawable2, Select, View, createRootElement, draw', selListener', setSelStates, uiHelpers, unpack)
import App.View.Util.D3 as D3
import Data.Array (zip)
import Data.Foldable (sequence_)
import Dict (Dict, toArrayWithKey)
import Effect (Effect)
import Util ((×))
import Util.Map (mapWithKey)

data MultiView = MultiView (Dict View)

instance Drawable MultiView where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec (selListener' figVal redraw)

instance Drawable2 MultiView where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: MultiView -> D3.Selection -> String -> Effect D3.Selection
createRootElement' (MultiView views) div childId = do
   sequence_ $ flip mapWithKey views \x view -> do
      unpack view \v -> createRootElement v div (childId <> "-" <> x)
   pure div

setSelStates' :: MultiView -> Select -> D3.Selection -> Effect Unit
setSelStates' (MultiView views) select rootElement = do
   children <- rootElement # D3.selectAll "svg"
   let xs = toArrayWithKey (×) views
   sequence_ $ flip map (zip children xs) \(elem × x × view) -> do
      unpack view \v -> setSelStates v (multiViewEntry x >>> select) elem

