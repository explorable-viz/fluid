module App.View.DocView where

import Prelude

import App.Util.Selector (docSel)
import App.View.Paragraph (Paragraph)
import App.View.Util (class Drawable, View, draw', drawView, selListener, uiHelpers)

newtype DocView = DocView
   { "title" :: String
   , doc :: Paragraph
   , view :: View
   }

instance Drawable DocView where
   draw { divId, view: DocView { title, doc, view } } figVal figView redraw = do
      drawView { divId, suffix: title, view } figVal figView redraw
      draw' uiHelpers { divId, suffix: title <> "-doc", view: doc } =<< selListener figVal redraw docSel

