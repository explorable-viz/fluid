module App.View.Paragraph where

import Prelude

import App.Util.Selector (ConstrArg, listElement)
import App.View.Util (class Viewable, Select, View, createElement, isLeaf, setSelection)
import App.View.Util.D3 (ElementType(..), create, createText)
import App.View.Util.D3 as D3
import Bind ((↦))
import DataType (cParagraph, f_fragments)
import Data.Array (mapWithIndex)
import Data.Foldable (all, sequence_)
import Effect (Effect)

data Paragraph = Paragraph (Array View)

instance Viewable Paragraph Unit where
   isLeaf (Paragraph views) = all isLeaf views

   createElement :: Unit -> Paragraph -> D3.Selection -> Effect D3.Selection
   createElement _ (Paragraph views) parent = do
      rootElement <- parent # create Div [ "class" ↦ "para-text" ]
      sequence_ $ views <#> \view -> do
         void $ createText rootElement " "
         createElement unit view rootElement
      pure rootElement

   setSelection :: ConstrArg -> Unit -> Paragraph -> Select -> D3.Selection -> Effect Unit
   setSelection sels _ (Paragraph views) select rootElement = do
      sequence_ $ flip mapWithIndex views \i view -> do
         child <- rootElement # D3.select (D3.nthChildOf D3.scope (i + 1))
         setSelection sels unit view (listElement i >>> sels cParagraph f_fragments >>> select) child
