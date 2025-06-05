module App.View.DocView where

import Prelude

import App.View.Paragraph (Paragraph)
import App.View.Util (class Drawable, class Drawable2, View, createRootElement, drawView, pack, setSelStates, unpack)
import App.View.Util.D3 (create, ElementType(..))
import App.View.Util.D3 as D3
import Bind ((↦))
import Effect (Effect)
import Util (spy)
import Web.Event.Internal.Types (Event)

newtype DocView = DocView
   { title :: String
   , doc :: Paragraph
   , view :: View
   }

instance Drawable DocView where
   draw { divId, view: DocView { title, doc, view } } figVal figView redraw = do
      drawView { divId, suffix: title, view } figVal figView redraw
      drawView { divId, suffix: spy "" identity "doc", view: pack doc } figVal figView redraw

instance Drawable2 DocView where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: DocView -> D3.Selection -> String -> Effect D3.Selection
createRootElement' (DocView { title, doc, view }) div childId = do
   rootElement <- div # create G [ "id" ↦ childId ]
   _ <- unpack view \v -> createRootElement v rootElement title -- view
   _ <- createRootElement doc rootElement (childId <> "-doc") -- doc
   pure rootElement

-- Endo 

setSelStates' :: DocView -> (Event -> Effect Unit) -> D3.Selection -> Effect Unit
setSelStates' (DocView { doc, view }) redraw rootElement = do
   _ <- unpack view \v -> setSelStates v redraw rootElement
   _ <- setSelStates doc redraw rootElement
   pure unit
