module App.View.DocView where

import Prelude

import App.View.Paragraph (Paragraph)
import App.View.Util (class Drawable, Select, View, createRootElement, setSelStates, unpack)
import App.View.Util.D3 as D3
import Data.Maybe (Maybe(..))
import Effect (Effect)

newtype DocView = DocView
   { doc :: Maybe Paragraph
   , view :: View
   }

instance Drawable DocView Unit where
   createRootElement _ = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: DocView -> D3.Selection -> Effect D3.Selection
createRootElement' (DocView { doc: Just doc, view }) parent = do
   rootElement <- parent # D3.create D3.G []
   void $ unpack view \v -> createRootElement (const []) v rootElement
   void $ createRootElement (const []) doc rootElement
   pure rootElement
createRootElement' (DocView { doc: Nothing, view }) parent = do
   unpack view \v -> createRootElement (const []) v parent

setSelStates' :: DocView -> Select -> D3.Selection -> Effect Unit
setSelStates' (DocView { doc: Just doc, view }) select rootElement = do
   viewElem <- rootElement # D3.select (D3.nthChild 1)
   void $ unpack view \v -> setSelStates v select viewElem
   docElem <- rootElement # D3.select (D3.nthChild 2)
   void $ setSelStates doc select docElem
setSelStates' (DocView { doc: Nothing, view }) select rootElement = do
   unpack view \v -> setSelStates v select rootElement
