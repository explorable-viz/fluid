module App.View.DocView where

import Prelude

import App.View.Paragraph (Paragraph)
import App.View.Util (class Drawable, class Drawable2, Select, View, createRootElement, draw', nthChild, selListener', setSelStates, uiHelpers, unpack)
import App.View.Util.D3 as D3
import Data.Maybe (Maybe(..))
import Effect (Effect)

newtype DocView = DocView
   { doc :: Maybe Paragraph
   , view :: View
   }

instance Drawable DocView where
   draw rSpec figVal _ redraw = do
      draw' uiHelpers rSpec (selListener' figVal redraw)

instance Drawable2 DocView where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: DocView -> D3.Selection -> Effect D3.Selection
createRootElement' (DocView { doc: Just doc, view }) div = do
   rootElement <- div # D3.create D3.G []
   _ <- unpack view \v -> createRootElement v rootElement
   _ <- createRootElement doc rootElement
   pure rootElement
createRootElement' (DocView { doc: Nothing, view }) div = do
   unpack view \v -> createRootElement v div

setSelStates' :: DocView -> Select -> D3.Selection -> Effect Unit
setSelStates' (DocView { doc: Just doc, view }) select rootElement = do
   viewElem <- rootElement # D3.select (nthChild 1)
   _ <- unpack view \v -> setSelStates v select viewElem
   docElem <- rootElement # D3.select (nthChild 2)
   _ <- setSelStates doc select docElem
   pure unit
setSelStates' (DocView { doc: Nothing, view }) select rootElement = do
   unpack view \v -> setSelStates v select rootElement
