module App.View.Paragraph where

import Prelude hiding (join)

import App.Util (class Reflect, Attrs, SelStates, Selectable, 𝕊, classes, contents, dict, from, isPersistent, isPrimary, isSecondary, isTransient, sel)
import App.Util.Selector (ViewSelSetter, SelSetter, listElement, paragraph)
import App.View.BarChart (BarChart)
import App.View.LineChart (LineChart)
import App.View.MatrixView (MatrixView(..), matrixRep)
import App.View.MultiView (MultiView(..))
import App.View.ScatterPlot (ScatterPlot)
import App.View.TableView (TableView(..), arrayDictToArray2, defaultFilter, headers)
import App.View.Util (class Drawable, class Drawable2, View, draw', pack, registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (create, datum, selectAll, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List ((:), List(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (snd)
import DataType (cBarChart, cCons, cLineChart, cLink, cMultiView, cNil, cParagraph, cScatterPlot, cText)
import Dict (Dict)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Util (type (×), error, (!), (×))
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
   from r = Paragraph (from <$> (from r :: Array (Val (SelStates 𝕊))))

instance Reflect (Val (SelStates 𝕊)) ParaFragment where
   from r = case r of
      Val _ _ (Constr c (Val α _ (Str s) : Nil)) | c == cText -> Text (s × α)
      Val _ _ (Constr c (_ : Nil))
         | c == cBarChart || c == cLineChart || c == cScatterPlot || c == cParagraph || c == cMultiView ->
              Viewable $ view r
      Val _ _ (Matrix _) -> Viewable $ view r

view :: Partial => Val (SelStates 𝕊) -> View
view v = case v of
   Val _ _ (Constr c (u : Nil))
      | c == cBarChart -> pack $ (dict from u :: BarChart)
      | c == cLineChart -> pack $ (dict from u :: LineChart)
      | c == cScatterPlot -> pack $ (dict from u :: ScatterPlot)
      | c == cParagraph -> pack $ (from u :: Paragraph)
      | c == cMultiView -> pack $ MultiView $ view <$> ((from u :: Dict (SelStates 𝕊 × Val (SelStates 𝕊))) # map snd)
      | c == cNil || c == cCons ->
           (pack $ TableView { title: "", filter: defaultFilter, colNames, rows })
           where
           records = dict identity <$> from u
           colNames = headers records
           rows = arrayDictToArray2 colNames records <#> map snd
   Val _ _ (Matrix r) -> pack $ MatrixView { title: "", matrix: matrixRep r }

type ParagraphElem = { i :: Int }
