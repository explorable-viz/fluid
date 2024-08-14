module App.View.LinkedText where

import Prelude

import App.Util (class Reflect, SelState, Selectable, 𝕊, ViewSelector, from)
import App.Util.Selector (linkedText, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Primitive (string, unpack)
import Val (Val)

foreign import drawLinkedText :: LinkedTextHelpers -> Renderer LinkedText Unit

type LinkedTextHelpers = {}
newtype LinkedText = LinkedText (Array (Selectable String))

drawLinkedText' :: Renderer LinkedText Unit
drawLinkedText' = drawLinkedText {}

instance Drawable LinkedText Unit where
   draw divId suffix redraw view viewState =
      drawLinkedText' { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw linkedTextSelector
      where
      linkedTextSelector :: ViewSelector LinkedTextElem
      linkedTextSelector { i } = linkedText <<< listElement i

instance Reflect (Val (SelState 𝕊)) LinkedText where
   from r = LinkedText (unpack string <$> from r)

-- unpackedStringify :: forall a. Tuple (Int + Number + String) a -> Tuple String a
-- unpackedStringify (Tuple x y) = Tuple (stringify x) y

-- stringify :: (Int + Number + String) -> String
-- stringify (Left n) = toString $ toNumber n
-- stringify (Right (Left n)) = toString n
-- stringify (Right (Right n)) = n

type LinkedTextElem = { i :: Int }