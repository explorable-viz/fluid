module App.View.MultiView where

import Prelude

import App.Util.Selector (ConstrArg, listElement)
import App.View.Util (class Viewable, Select, View, createElement, setSelection)
import App.View.Util.D3 (create)
import App.View.Util.D3 as D3
import DataType (cMultiView, f_views)
import Data.Foldable (sequence_)
import Data.FunctorWithIndex (mapWithIndex)
import Effect (Effect)

data MultiView = MultiView (Array View)

instance Viewable MultiView Unit where
   isLeaf = const false

   createElement :: Unit -> MultiView -> D3.Selection -> Effect D3.Selection
   createElement _ (MultiView views) parent = do
      rootElement <- parent # create D3.Div []
      sequence_ $ views <#> \view ->
         createElement unit view rootElement
      pure rootElement

   setSelection :: ConstrArg -> Unit -> MultiView -> Select -> D3.Selection -> Effect Unit
   setSelection sels _ (MultiView views) select rootElement = do
      sequence_ $ flip mapWithIndex views \i view -> do
         child <- rootElement # D3.select (D3.nthChildOf D3.scope (i + 1))
         setSelection sels unit view (listElement i >>> sels cMultiView f_views >>> select) child
