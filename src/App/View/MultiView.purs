module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class View, Select, View', createElement, setSelection, unpack)
import App.View.Util.D3 (create)
import App.View.Util.D3 as D3
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import Dict (Dict)
import Effect (Effect)
import Util ((×))
import Util.Map (toUnfoldable)

data MultiView = MultiView (Dict View')

instance View MultiView Unit where
   -- create views in fixed order, so can access positionally in setSelection and map back to keys
   createElement :: Unit -> MultiView -> D3.Selection -> Effect D3.Selection
   createElement _ (MultiView views) parent = do
      rootElement <- parent # create D3.Div []
      sequence_ $ (toUnfoldable views :: Array _) <#> \(_ × view) ->
         unpack view \v -> createElement unit v rootElement
      pure rootElement

   setSelection :: Unit -> MultiView -> Select -> D3.Selection -> Effect Unit
   setSelection _ (MultiView views) select rootElement =
      sequence_ $
         flip mapWithIndex (toUnfoldable views) \i (x × view) -> do
            child <- rootElement # D3.select ("svg" <> D3.nthChild (i + 1)) -- TODO: remove 'svg'
            void $ unpack view \v -> setSelection unit v (multiViewEntry x >>> select) child
