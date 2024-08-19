module App.View.LinkedText where

import Prelude

import App.Util (class Reflect, SelState, Selectable, 𝕊)
import App.Util.Selector (ViewSelector, linkedText)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Number.Format (toString)
import Primitive (intOrNumber, unpack)
import Util (type (+), type (×), (×))
import Val (Val)

foreign import drawLinkedText :: LinkedTextHelpers -> Renderer LinkedText

newtype LinkedText = LinkedText (Selectable String)

type LinkedTextHelpers =
   { test_field :: String
   }

linkedTextHelpers :: LinkedTextHelpers
linkedTextHelpers =
   { test_field: "test"
   }

instance Drawable LinkedText where
   draw rSpec figVal _ redraw =
      drawLinkedText linkedTextHelpers uiHelpers rSpec =<< selListener figVal redraw linkedTextSelector
      where
      linkedTextSelector :: ViewSelector LinkedText
      linkedTextSelector _ = linkedText

instance Reflect (Val (SelState 𝕊)) LinkedText where
   from r = LinkedText (unpackedStringify $ unpack intOrNumber r)

unpackedStringify :: forall a. (Int + Number) × a -> String × a
unpackedStringify (x × y) = stringify x × y

stringify :: (Int + Number) -> String
stringify (Left n) = toString $ toNumber n
stringify (Right n) = toString n

type LinkedTextElem = { i :: Int }
