module App.View.Paragraph where

import Prelude

import App.Util.Selector (constrArg, listElement)
import App.View.Util (class Viewable, Select, View, createElement, setSelection, unpack)
import App.View.Util.D3 (ElementType(..), create, createText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import DataType (cParagraph)
import Effect (Effect)

data Paragraph = Paragraph (Array View)

instance Viewable Paragraph Unit where
   createElement :: Unit -> Paragraph -> D3.Selection -> Effect D3.Selection
   createElement _ (Paragraph views) parent = do
      rootElement <- parent # create Div [ "class" ↦ "para-text" ]
      sequence_ $ views <#> \view -> do
         void $ createText rootElement " "
         unpack view \v -> createElement unit v rootElement

      pure rootElement

   setSelection :: Unit -> Paragraph -> Select -> D3.Selection -> Effect Unit
   setSelection _ (Paragraph views) select rootElement = do
      sequence_ $ flip mapWithIndex views \i view -> do
         child <- rootElement # D3.select (D3.nthChildOf D3.scope (i + 1))
         unpack view \v -> setSelection unit v (select <<< constrArg cParagraph 0 <<< listElement i) child
