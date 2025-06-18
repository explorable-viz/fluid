module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class Drawable, Select, View, createRootElement, setSelStates, unpack)
import App.View.Util.D3 (create)
import App.View.Util.D3 as D3
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import Dict (Dict)
import Effect (Effect)
import Util (type (×), (×))
import Util.Map (toUnfoldable)

data MultiView = MultiView (Dict View)

instance Drawable MultiView where
   createRootElement _ = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: MultiView -> D3.Selection -> Effect D3.Selection
createRootElement' (MultiView views) parent = do
   rootElement <- parent # create D3.G []
   sequence_ $ flip map views \view -> do
      unpack view \v -> createRootElement [] v rootElement
   pure rootElement

setSelStates' :: MultiView -> Select -> D3.Selection -> Effect Unit
setSelStates' (MultiView views) select rootElement = do
   sequence_ $
      flip mapWithIndex (toUnfoldable views :: Array (String × View)) \i (x × view) -> do
         elem <- rootElement # D3.select ("svg" <> D3.nthChild (i + 1))
         void $ unpack view \v -> setSelStates v (multiViewEntry x >>> select) elem
