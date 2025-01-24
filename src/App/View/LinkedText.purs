module App.View.LinkedText where

import Prelude

import App.Util (class Reflect, Attrs, SelState, Selectable, 𝕊, classes, from, isPersistent, isPrimary, isSecondary, isTransient)
import App.Util.Selector (linkedText, listElement, ViewSelSetter)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (ElementType(..), create, datum, selectAll, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Array (length)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Primitive (string, unpack)
import Util ((!))
import Val (Val)
import Web.Event.EventTarget (EventListener)

newtype LinkedText = LinkedText (Array (Selectable String))

contents :: Selectable String -> String
contents = fst

accessAnn :: Selectable String -> SelState 𝕊
accessAnn = snd

instance Drawable LinkedText where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec =<< selListener figVal redraw linkedTextSelector
      where
      linkedTextSelector :: ViewSelSetter LinkedTextElem
      linkedTextSelector { i } = linkedText <<< listElement i

setSelState :: LinkedText -> EventListener -> D3.Selection -> Effect Unit
setSelState (LinkedText elems) redraw rootElement = do
   elems' <- rootElement # selectAll ".linked-text"
   logShow (length elems')
   for_ elems' \elem -> do
      elem' :: LinkedTextElem <- datum elem
      log "About to set attrs"
      log $ "Elem': " <> (show elem')
      elem # setStyles (textAttrs elem') >>= registerMouseListeners redraw
      log "set attrs"
   log "ok"
   where
   textAttrs :: LinkedTextElem -> Attrs
   textAttrs { i } =
      [ "border-bottom" ↦ border
      , "background" ↦ background
      , "color" ↦ color
      ]
      where
      sel = accessAnn (elems ! i)

      border :: String
      border
         | isTransient sel = "1px solid blue"
         | otherwise = "none"

      background :: String
      background
         | isPrimary sel && isPersistent sel = "#93E9BE"
         | isSecondary sel && isPersistent sel = "rgb(226, 226, 226)"
         | otherwise = "white"

      color :: String
      color
         | isPrimary sel && isTransient sel = "blue"
         | isSecondary sel && isTransient sel = "royalblue"
         | otherwise = "black"

createRootElement :: LinkedText -> D3.Selection -> String -> Effect D3.Selection
createRootElement (LinkedText elems) div childId = do
   rootElement <- div # create Text [ classes [ "linked-text-parent" ], "id" ↦ childId ]
   forWithIndex_ elems \i elem -> do
      elem' <- rootElement # create Text [ classes [ "linked-text" ], "id" ↦ childId ]
      elem' # setText (contents elem) >>= setDatum { i }
   pure rootElement

instance Drawable2 LinkedText where
   createRootElement = createRootElement
   setSelState = setSelState

instance Reflect (Val (SelState 𝕊)) LinkedText where
   from r = LinkedText (unpack string <$> ((from r)))

type LinkedTextElem = { i :: Int }

solidBorder :: String
solidBorder = "1px solid blue"

