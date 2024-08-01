module App.View.LinkedText where

import App.View.Util (Renderer)
import Prelude

foreign import drawLinkedText :: LinkedTextHelpers -> Renderer LinkedText Unit

newtype LinkedText = LinkedText {
   
}

type LinkedTextHelpers = {} 