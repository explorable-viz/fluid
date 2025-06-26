module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class Drawable, Select, View', createRootElement, setSelStates, unpack)
import App.View.Util.D3 (create)
import App.View.Util.D3 as D3
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import Dict (Dict)
import Effect (Effect)
import Util (type (×), (×))
import Util.Map (toUnfoldable)

data MultiView = MultiView (Dict View')

instance Drawable MultiView Unit Unit where
   createRootElement _ = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: MultiView -> D3.Selection -> Effect D3.Selection
createRootElement' (MultiView views) parent = do
   rootElement <- parent # create D3.G []
   sequence_ $ views' <#> \(_ × view) ->
      unpack view \v -> createRootElement (const const) v rootElement
   pure rootElement
   where
   -- create views in fixed order so we can access positionally in setSelStates and map back to keys
   views' :: Array (String × View')
   views' = toUnfoldable views

setSelStates' :: MultiView -> Select -> D3.Selection -> Effect Unit
setSelStates' (MultiView views) select rootElement =
   sequence_ $
      flip mapWithIndex views' \i (x × view) -> do
         child <- rootElement # D3.select ("svg" <> D3.nthChild (i + 1)) -- TODO: remove 'svg'
         void $ unpack view \v -> setSelStates v (multiViewEntry x >>> select) child
   where
   views' :: Array (String × View')
   views' = toUnfoldable views
