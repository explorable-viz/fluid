module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class Drawable, Select, View, createRootElement, nthChild, setSelStates, unpack)
import App.View.Util.D3 (create)
import App.View.Util.D3 as D3
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import Dict (Dict)
import Effect (Effect)
import Util (type (×), (×))
import Util.Map (mapWithKey, toUnfoldable)

data MultiView = MultiView (Dict View)

instance Drawable MultiView where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: MultiView -> D3.Selection -> Effect D3.Selection
createRootElement' (MultiView views) div = do
   rootElement <- div # create D3.G []
   sequence_ $ flip mapWithKey views \_x view -> do
      unpack view \v -> createRootElement v rootElement
   pure rootElement

setSelStates' :: MultiView -> Select -> D3.Selection -> Effect Unit
setSelStates' (MultiView views) select rootElement = do
   sequence_ $
      ( flip mapWithIndex (toUnfoldable views :: Array (String × View)) \i (x × view) -> do
           elem <- rootElement # D3.select ("svg" <> nthChild (i + 1))
           void $ unpack view \v -> setSelStates v (multiViewEntry x >>> select) elem
      )
