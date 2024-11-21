module App.View.LinkedText where

import Prelude

import App.Util (class Reflect, SelState, Selectable, 𝕊, from)
import App.Util.Selector (linkedText, listElement, ViewSelSetter)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Either (Either(..))
import Data.Tuple (Tuple, fst)
import Primitive (Explanation(..), linkedTextEntry, string, unpack)
import Util (type (+), (×))
import Val (Val)

foreign import drawLinkedText :: LinkedTextHelpers -> Renderer LinkedText

type LinkedTextHelpers =
   { explanation :: Selectable String + Selectable (Explanation (SelState 𝕊)) -> String
   , contents :: Selectable String + Selectable (Explanation (SelState 𝕊)) -> String
   , accessAnn :: Selectable String + Selectable (Explanation (SelState 𝕊)) -> SelState 𝕊
   }

newtype LinkedText = LinkedText (Array (Selectable String + Selectable (Explanation (SelState 𝕊))))

drawLinkedText' :: Renderer LinkedText
drawLinkedText' = drawLinkedText linkedTextHelpers

linkedTextHelpers :: LinkedTextHelpers
linkedTextHelpers =
   { explanation
   , contents
   , accessAnn
   }
   where
   explanation :: Selectable String + Selectable (Explanation (SelState 𝕊)) -> String
   explanation = case _ of
      Left s -> fst s
      Right (Explanation _ expl _ × _) -> expl

   contents :: Selectable String + Selectable (Explanation (SelState 𝕊)) -> String
   contents = case _ of
      Left s -> fst s
      Right (Explanation _ _ v × _) -> fst (unpack string v)

   accessAnn :: Selectable String + Selectable (Explanation (SelState 𝕊)) -> SelState 𝕊
   accessAnn = case _ of
      Left (_ × α) -> α
      Right (Explanation α _ _ × _) -> α

instance Drawable LinkedText where
   draw rSpec figVal _ redraw =
      drawLinkedText linkedTextHelpers uiHelpers rSpec =<< selListener figVal redraw linkedTextSelector
      where
      linkedTextSelector :: ViewSelSetter LinkedTextElem
      linkedTextSelector { i } = linkedText <<< listElement i

exch :: forall a. Tuple (Either String (Explanation a)) a -> Either (Tuple String a) (Tuple (Explanation a) a)
exch (e × a) = case e of
   Left s -> Left (s × a)
   Right ex -> Right (ex × a)

instance Reflect (Val (SelState 𝕊)) LinkedText where
   from r = LinkedText (exch <$> unpack linkedTextEntry <$> ((from r)))

type LinkedTextElem = { i :: Int }
