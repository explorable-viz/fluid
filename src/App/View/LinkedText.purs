module App.View.LinkedText where

import Prelude

import App.Util (class Reflect, Attrs, SelState, Selectable, 𝕊, from, isTransient)
import App.Util.Selector (linkedText, listElement, ViewSelSetter)
import App.View.Util (class Drawable, Renderer, registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (datum, selectAll, setAttrs)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple (Tuple, fst)
import Effect (Effect)
import Effect.Console (log)
import Primitive (Explanation(..), linkedTextEntry, string, unpack)
import Util (type (+), (×), (!))
import Val (Val)
import Web.Event.EventTarget (EventListener)

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

_setSelState :: LinkedText -> EventListener -> D3.Selection -> Effect Unit
_setSelState (LinkedText elems) _redraw rootElement = do
   elems' <- rootElement # selectAll ".linked-text"
   for_ elems' \elem -> do
      elem' <- datum elem
      elem # setAttrs (_textAttrs elem') >>= registerMouseListeners _redraw
   log "ok"
   where
   _textAttrs :: LinkedTextElem -> Attrs
   _textAttrs { i } = 
      [  "border-right" ↦ border (hasBorder i)
      ]
   border :: Boolean -> String
   border b = if b then "1px solid blue" else "none"
   hasBorder :: Int -> Boolean
   hasBorder i = isTransient $ accessAnn (elems ! i)

instance Reflect (Val (SelState 𝕊)) LinkedText where
   from r = LinkedText (exch <$> unpack linkedTextEntry <$> ((from r)))

type LinkedTextElem = { i :: Int }
