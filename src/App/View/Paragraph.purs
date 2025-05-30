module App.View.Paragraph where

import Prelude hiding (join)

import App.Util (Attrs, Selectable, classes, inert, isPersistent, isPrimary, isSecondary, isTransient, sel)
import App.Util.Selector (ViewSelSetter, SelSetter, listElement, paragraph)
import App.View.Util (class Drawable, class Drawable2, View, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (create, datum, selectAll, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Array (foldl)
import Data.Array.Partial (head, tail)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List ((:), List(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import DataType (cText)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Util (error, (!), (×))
import Val (BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

newtype Paragraph = Paragraph (Array ParaFragment)

data ParaFragment = Text (Array (Selectable String)) | Viewable View

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

getText :: Array ParaFragment -> Int -> Selectable String
getText elems i = case elems ! i of
   Text s -> formatFragments s × inert
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
textContents (Text s) = formatFragments s
textContents (Viewable _) = error "unimplemented"

instance Drawable2 Paragraph where
   createRootElement = createRootElement
   setSelStates = setSelStates

type ParagraphElem = { i :: Int }

formatFragments :: Array (Selectable String) -> String
formatFragments fragments =
   case fragments of
      [] -> ""
      [ s × _ ] -> s
      _ -> unsafePartial $ foldl (\acc s -> acc <> " " <> s) (fst $ head fragments) (map fst $ tail fragments)

