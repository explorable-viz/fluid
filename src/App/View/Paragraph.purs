module App.View.Paragraph where

import Prelude

import App.Util.Selector (SelSetter, constrArg, docElement, listElement)
import App.View.Util (class View, Select, View', createElement, setSelection, unpack)
import App.View.Util.D3 (create, ElementType(..))
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import DataType (cParagraph)
import Effect (Effect)
import Val (Val)

data Paragraph = Paragraph Boolean (Array View')

instance View Paragraph Unit where
   createElement _ = createRootElement'
   setSelection _ = setSelStates'

createRootElement' :: Paragraph -> D3.Selection -> Effect D3.Selection
createRootElement' (Paragraph _ views) parent = do
   rootElement <- parent # create Div [ "class" ↦ "para-text" ]
   sequence_ $ flip map views \view -> do
      unpack view \v -> createElement unit v rootElement
   pure rootElement

setSelStates' :: Paragraph -> Select -> D3.Selection -> Effect Unit
setSelStates' (Paragraph isDoc views) select rootElement = do
   sequence_ $ flip mapWithIndex views \i view -> do
      unpack view \v -> setSelection unit v (select <<< lift i) rootElement
   where
   lift :: Int -> SelSetter Val Val
   lift i = if isDoc then docElement i else constrArg cParagraph 0 <<< listElement i
