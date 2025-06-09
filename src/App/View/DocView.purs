module App.View.DocView where

import Prelude

import App.View.Paragraph (Paragraph)
import App.View.Util (class Drawable, class Drawable2, Select, View, createRootElement, draw', selListener', setSelStates, uiHelpers, unpack)
-- import App.View.Util.D3 (create, ElementType(..))
import App.View.Util.D3 as D3
-- import Bind ((↦))
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

createRootElement' :: DocView -> D3.Selection -> String -> Effect D3.Selection
createRootElement' (DocView { doc: Just doc, view }) div childId = do
   rootElement <- unpack view \v -> createRootElement v div childId -- view
   _ <- createRootElement doc div (childId <> "-doc") -- doc
   pure rootElement
createRootElement' (DocView { doc: Nothing, view }) div childId = do
   unpack view \v -> createRootElement v div childId -- view

setSelStates' :: DocView -> Select -> D3.Selection -> Effect Unit
setSelStates' (DocView { doc: Just doc, view }) select rootElement = do
   _ <- unpack view \v -> setSelStates v select rootElement
   _ <- setSelStates doc select rootElement
   pure unit
setSelStates' (DocView { doc: Nothing, view }) select rootElement = do
   unpack view \v -> setSelStates v select rootElement
