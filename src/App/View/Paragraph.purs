module App.View.Paragraph where

import Prelude

import App.Util (class Reflect, Attrs, SelState, Selectable, 𝕊, classes, contents, from, isPersistent, isPrimary, isSecondary, isTransient, sel)
import App.Util.Selector (paragraph, listElement, ViewSelSetter)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (ElementType(..), create, datum, selectAll, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List ((:), List(..))
import Data.Tuple (fst)
import DataType (cText)
import Effect (Effect)
import Primitive (ToFrom, typeError, unpack)
import Util (error, (!), (×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

newtype Paragraph a = Paragraph (Array (TextFragment a))

data TextFragment a = TextFragment (Selectable String) | Link (Val a) (Selectable String)

instance Drawable (Paragraph (SelState 𝕊)) where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec =<< selListener figVal redraw paragraphSelector
      where
      paragraphSelector :: ViewSelSetter ParagraphElem
      paragraphSelector { i } = paragraph <<< listElement i

setSelState :: forall a. Paragraph a -> EventListener -> D3.Selection -> Effect Unit
setSelState (Paragraph elems) redraw rootElement = do
   elems' <- rootElement # selectAll ".text-fragment"
   for_ elems' \elem -> do
      elem' :: ParagraphElem <- datum elem
      elem # setStyles (textAttrs elem') >>= registerMouseListeners redraw
   where
   textAttrs :: ParagraphElem -> Attrs
   textAttrs { i } =
      [ "border-bottom" ↦ border
      , "background" ↦ background
      , "color" ↦ color
      ]
      where
      tf = getText elems i
      sel' = sel tf

      border :: String
      border
         | isTransient sel' = "1px solid blue"
         | otherwise = "none"

      background :: String
      background
         | isPrimary sel' && isPersistent sel' = "#93E9BE"
         | isSecondary sel' && isPersistent sel' = "rgb(226, 226, 226)"
         | otherwise = "white"

      color :: String
      color
         | isPrimary sel' && isTransient sel' = "blue"
         | isSecondary sel' && isTransient sel' = "royalblue"
         | otherwise = "black"

getText :: forall a. Array (TextFragment a) -> Int -> Selectable String
getText elems i = case elems ! i of
   TextFragment s -> s
   Link _ s -> s

createRootElement :: Paragraph (SelState 𝕊) -> D3.Selection -> String -> Effect D3.Selection
createRootElement (Paragraph elems) div childId = do
   rootElement <- div # create Text [ classes [ "paragraph" ], "id" ↦ childId ]
   forWithIndex_ elems (mkElem rootElement)
   pure rootElement
   where
   mkElem :: D3.Selection -> Int -> TextFragment (SelState 𝕊) -> Effect D3.Selection
   mkElem root i (TextFragment elem) = do
      elem' <- root # create Text [ classes [ "text-fragment" ], "id" ↦ childId ]
      elem' # setText (contents elem) >>= setDatum { i }
   mkElem _root _i (Link _ _) = error "todo"

instance Drawable2 (Paragraph (SelState 𝕊)) where
   createRootElement = createRootElement
   setSelState = setSelState

instance Reflect (Val (SelState 𝕊)) (Paragraph (SelState 𝕊)) where
   from r = Paragraph (fst <$> unpack textFragment <$> (from r))

type ParagraphElem = { i :: Int }

textFragment :: ToFrom (TextFragment (SelState 𝕊)) (SelState 𝕊)
textFragment =
   { pack: case _ of
        TextFragment (s × α) -> Constr cText ((Val α (Str s)) : Nil)
        Link _v (s × α) -> Constr cText ((Val α (Str s)) : Nil)
   , unpack: case _ of
        Constr c (Val α (Str s) : Nil) | c == cText -> TextFragment (s × α)
        v -> typeError v "TextFragment"
   }
