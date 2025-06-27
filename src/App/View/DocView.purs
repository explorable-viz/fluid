module App.View.DocView where

import Prelude

import App.View.Paragraph (Paragraph)
import App.View.Util (class View2, Select, View', createElement, setSelection, unpack)
import App.View.Util.D3 as D3
import Data.Maybe (Maybe(..))
import Effect (Effect)

newtype DocView = DocView
   { doc :: Maybe Paragraph
   , view :: View'
   }

instance View2 DocView Unit where
   createElement :: Unit -> DocView -> D3.Selection -> Effect D3.Selection
   createElement _ (DocView { doc: Just doc, view }) parent = do
      rootElement <- parent # D3.create D3.G []
      void $ unpack view \v -> createElement unit v rootElement
      void $ createElement unit doc rootElement
      pure rootElement
   createElement _ (DocView { doc: Nothing, view }) parent = do
      unpack view \v -> createElement unit v parent

   setSelection :: Unit -> DocView -> Select -> D3.Selection -> Effect Unit
   setSelection _ (DocView { doc: Just doc, view }) select rootElement = do
      viewElem <- rootElement # D3.select (D3.nthChild 1)
      void $ unpack view \v -> setSelection unit v select viewElem
      docElem <- rootElement # D3.select (D3.nthChild 2)
      void $ setSelection unit doc select docElem
   setSelection _ (DocView { doc: Nothing, view }) select rootElement = do
      unpack view \v -> setSelection unit v select rootElement
