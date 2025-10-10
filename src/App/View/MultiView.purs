module App.View.MultiView where

import Prelude

import App.Util.Selector (constrArg, dictVal)
import App.View.Util (class Viewable, Select, View, createElement, setSelection)
import App.View.Util.D3 (create)
import App.View.Util.D3 as D3
import Data.Foldable (sequence_)
import Data.FunctorWithIndex (mapWithIndex)
import DataType (cMultiView)
import Dict (Dict)
import Effect (Effect)
import Util (type (×), (×))
import Util.Map (toUnfoldable)

data MultiView = MultiView (Dict (View × View))

instance Viewable MultiView Unit where
   isLeaf = const false

   createElement :: Unit -> MultiView -> D3.Selection -> Effect D3.Selection
   createElement _ (MultiView views) parent = do
      rootElement <- parent # create D3.Div []
      sequence_ $ (toUnfoldable views :: Array _) <#> \(_ × _ × view) ->
         createElement unit view rootElement
      pure rootElement

   setSelection :: Unit -> MultiView -> Select -> D3.Selection -> Effect Unit
   setSelection _ (MultiView views) select rootElement = do
      sequence_ $ flip mapWithIndex (toUnfoldable views :: Array _) \i (x × _ × view) -> do
         child <- rootElement # D3.select (D3.nthChildOf D3.scope (i + 1))
         setSelection unit view (dictVal x >>> constrArg cMultiView 0 >>> select) child
