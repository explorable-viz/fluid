module App.View.DocView where

import Prelude

import App.View.Paragraph (Paragraph)
import App.View.Util (class Drawable, View, drawView, pack)
import Util (spy)

newtype DocView = DocView
   { title :: String
   , doc :: Paragraph
   , view :: View
   }

instance Drawable DocView where
   draw { divId, view: DocView { title, doc, view } } figVal figView redraw = do
      drawView { divId, suffix: title, view } figVal figView redraw
      drawView { divId, suffix: spy "" identity "doc", view: pack doc } figVal figView redraw

