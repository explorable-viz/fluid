module App.View.Paragraph where

import Prelude hiding (join)

import App.Util (class Reflect, Attrs, SelStates, Selectable, 𝕊, classes, contents, from, isPersistent, isPrimary, isSecondary, isTransient, sel)
import App.Util.Selector (ViewSelSetter, SelSetter, listElement, paragraph)
import App.View.Util (class Drawable, class Drawable2, View, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (create, datum, selectAll, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (foldr, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List ((:), List(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import DataType (cLink, cText)
import Doc (DocOpt(..))
import Effect (Effect)
import Lattice (bot, join)
import Partial.Unsafe (unsafePartial)
import Primitive (ToFrom, typeError, unpack)
import Util (error, (!), (×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

newtype Paragraph a = Paragraph (Array (TextFragment a))

data TextFragment a = Text (Selectable String) | Link (Val a) (Selectable String) | Viewable View

instance Drawable (Paragraph (SelStates 𝕊)) where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec =<< selListener figVal redraw paragraphSelector
      where
      paragraphSelector :: ViewSelSetter ParagraphElem
      paragraphSelector { i } = selTextFragment { i }

selTextFragment :: ViewSelSetter ParagraphElem
selTextFragment { i } = fragment >>> listElement i >>> paragraph
   where
   fragment :: SelSetter Val Val
   fragment δv = unsafePartial $ case _ of
      Val α doc (Constr c (v : Nil)) | c == cText ->
         first (\v' -> Val α doc (Constr c (v' : Nil))) (δv v)
      Val α doc (Constr c (v1 : v2 : Nil)) | c == cLink ->
         first (\v1' -> Val α doc (Constr c (v1' : v2 : Nil))) (δv v1)

getText :: Array (TextFragment (SelStates 𝕊)) -> Int -> Selectable String
getText elems i = case elems ! i of
   Text s -> s
   Link v (s × _) -> (s × α)
      where
      α = foldr join bot v
   Viewable _ -> error "Unimplemented"

setSelStates :: Paragraph (SelStates 𝕊) -> EventListener -> D3.Selection -> Effect Unit
setSelStates (Paragraph elems) redraw rootElement = do
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

createRootElement :: Paragraph (SelStates 𝕊) -> D3.Selection -> String -> Effect D3.Selection
createRootElement (Paragraph elems) div childId = do
   rootElement <- div # create D3.Text [ classes [ "paragraph" ], "id" ↦ childId ]
   forWithIndex_ elems (mkElem rootElement)
   pure rootElement
   where
   mkElem :: D3.Selection -> Int -> TextFragment (SelStates 𝕊) -> Effect D3.Selection
   mkElem root i elem = do
      elem' <- root # create D3.Text [ classes [ "text-fragment" ] ]
      elem' # setText (linkContents elem) >>= setDatum { i }

linkContents :: TextFragment (SelStates 𝕊) -> String
linkContents (Text s) = contents s
linkContents (Link _ (s × _)) = s
linkContents (Viewable _) = error "unimplemented"

instance Drawable2 (Paragraph (SelStates 𝕊)) where
   createRootElement = createRootElement
   setSelStates = setSelStates

instance Reflect (Val (SelStates 𝕊)) (Paragraph (SelStates 𝕊)) where
   from r = Paragraph (fst <$> unpack textFragment <$> (from r))

type ParagraphElem = { i :: Int }

textFragment :: ToFrom (TextFragment (SelStates 𝕊)) (SelStates 𝕊)
textFragment =
   { pack: case _ of
        Text (s × α) -> Constr cText ((Val α None (Str s)) : Nil)
        Link v (s × α') -> Constr cLink (v : Val α' None (Str s) : Nil)
        Viewable _ -> error "unimplemented"
   , unpack: case _ of
        Constr c (Val α _ (Str s) : Nil) | c == cText -> Text (s × α)
        Constr c (Val α doc v : (Val α' _ (Str s) : Nil)) | c == cLink -> Link (Val α doc v) (s × α')
        v -> typeError v "TextFragment"
   }

