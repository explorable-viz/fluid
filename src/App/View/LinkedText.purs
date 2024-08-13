module App.View.LinkedText where

import Prelude

import App.Util (class Reflect, SelState, Selectable, ViewSelector, 𝕊)
import App.Util.Selector (linkedText)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..))
import Primitive (intOrNumberOrString, unpack)
import Util (type (+))
import Val (Val)

foreign import drawLinkedText :: LinkedTextHelpers -> Renderer LinkedText Unit

type LinkedTextHelpers = { test_field :: String }
newtype LinkedText = LinkedText (Selectable String)

drawLinkedText' :: Renderer LinkedText Unit
drawLinkedText' = drawLinkedText { test_field: "test" }

instance Drawable LinkedText Unit where
   draw divId suffix redraw view viewState =
      drawLinkedText' { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw linkedTextSelector
      where
      linkedTextSelector :: ViewSelector LinkedText
      linkedTextSelector _ = linkedText

instance Reflect (Val (SelState 𝕊)) LinkedText where
   from r = LinkedText (unpackedStringify $ unpack intOrNumberOrString r)

unpackedStringify :: forall a. Tuple (Int + Number + String) a -> Tuple String a
unpackedStringify (Tuple x y) = Tuple (stringify x) y

stringify :: (Int + Number + String) -> String
stringify (Left n) = toString $ toNumber n
stringify (Right (Left n)) = toString n
stringify (Right (Right n)) = n

type LinkedTextElem = { i :: Int }