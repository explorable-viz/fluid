module App.View.Paragraph where

import Prelude hiding (join)

import App.Util (class Reflect, Attrs, SelStates, Selectable, 𝕊, classes, contents, from, isPersistent, isPrimary, isSecondary, isTransient, sel)
import App.Util.Selector (ViewSelSetter, SelSetter, listElement, paragraph)
import App.View.Util (class Drawable, class Drawable2, View, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (create, datum, selectAll, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List ((:), List(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import DataType (cLink, cText)
import Doc (DocOpt(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Primitive (ToFrom, typeError, unpack)
import Util (error, (!), (×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

newtype Paragraph = Paragraph (Array ParaFragment)

data ParaFragment = Text (Selectable String) | Viewable View

instance Drawable Paragraph where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec =<< selListener figVal redraw paragraphSelector
      where
      paragraphSelector :: ViewSelSetter ParagraphElem
      paragraphSelector { i } = selParaFragment { i }

selParaFragment :: ViewSelSetter ParagraphElem
selParaFragment { i } = fragment >>> listElement i >>> paragraph
   where
   fragment :: SelSetter Val Val
   fragment δv = unsafePartial $ case _ of
      Val α doc (Constr c (v : Nil)) | c == cText ->
         first (\v' -> Val α doc (Constr c (v' : Nil))) (δv v)
      Val α doc (Constr c (v1 : v2 : Nil)) | c == cLink ->
         first (\v1' -> Val α doc (Constr c (v1' : v2 : Nil))) (δv v1)

getText :: Array ParaFragment -> Int -> Selectable String
getText elems i = case elems ! i of
   Text s -> s
   Viewable _ -> error "Unimplemented"

setSelStates :: Paragraph -> EventListener -> D3.Selection -> Effect Unit
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

createRootElement :: Paragraph -> D3.Selection -> String -> Effect D3.Selection
createRootElement (Paragraph elems) div childId = do
   rootElement <- div # create D3.Text [ classes [ "paragraph" ], "id" ↦ childId ]
   forWithIndex_ elems (mkElem rootElement)
   pure rootElement
   where
   mkElem :: D3.Selection -> Int -> ParaFragment -> Effect D3.Selection
   mkElem root i elem = do
      elem' <- root # create D3.Text [ classes [ "text-fragment" ] ]
      elem' # setText (textContents elem) >>= setDatum { i }

textContents :: ParaFragment -> String
textContents (Text s) = contents s
textContents (Viewable _) = error "unimplemented"

instance Drawable2 Paragraph where
   createRootElement = createRootElement
   setSelStates = setSelStates

instance Reflect (Val (SelStates 𝕊)) Paragraph where
   from r = Paragraph (fst <$> unpack textFragment <$> (from r))

type ParagraphElem = { i :: Int }

textFragment :: ToFrom ParaFragment (SelStates 𝕊)
textFragment =
   { pack: case _ of
        Text (s × α) -> Constr cText ((Val α None (Str s)) : Nil)
        Viewable _ -> error "unimplemented"
   , unpack: case _ of
        Constr c (Val α _ (Str s) : Nil) | c == cText -> Text (s × α)
        v -> typeError v "ParaFragment"
   }

