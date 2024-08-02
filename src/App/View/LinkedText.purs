module App.View.LinkedText where

import Prelude

import App.Util (Selectable, ViewSelector)
import App.Util.Selector (linkedText, textElem)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Either (Either)

foreign import drawLinkedText :: LinkedTextHelpers -> Renderer LinkedText Unit



newtype LinkedText = LinkedText
   {
      contents :: Array TextElem
   }

newtype TextElem = TextElem (Either String Quote)

newtype Quote = Quote { val :: Selectable String }

type LinkedTextHelpers = {}

drawLinkedText' :: Renderer LinkedText Unit
drawLinkedText' = drawLinkedText {}

instance Drawable LinkedText Unit where
   draw divId suffix redraw view viewState = 
      drawLinkedText' { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw linkedTextSelector
      where
      linkedTextSelector :: ViewSelector LinkedTextElem
      linkedTextSelector { i } = textElem i >>> linkedText
type LinkedTextElem = { i :: Int }