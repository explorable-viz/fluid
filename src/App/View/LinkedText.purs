module App.View.LinkedText where

import Prelude

import App.Util (class Reflect, SelState, Selectable, ViewSelector, 𝕊, from, record)
import App.Util.Selector (linkedText, textElem)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Either (Either(..))
import DataType (f_contents, f_val)
import Dict (Dict)
import Primitive (string, unpack)
import Util (error)
import Util.Map (get)
import Val (Val)

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

instance Reflect (Dict (Val (SelState 𝕊))) LinkedText where
   from r = LinkedText {
      contents : record from <$> from (get f_contents r)
   }

instance Reflect (Dict (Val (SelState 𝕊))) TextElem where
   from _r = error "todo"

instance Reflect (Dict (Val (SelState 𝕊))) Quote where
   from r = Quote {
      val : unpack string (get f_val r)
   }

type LinkedTextElem = { i :: Int }