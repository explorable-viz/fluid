module App.View.Paragraph where

import Prelude

import App.Util.Selector (docSel)
import App.View.Util (class Drawable, View, drawView)
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import Data.Newtype (class Newtype)

newtype Paragraph = Paragraph (Array View)

instance Drawable Paragraph where
   draw { divId, view: Paragraph views } figVal figView redraw = sequence_ $ flip mapWithIndex views \i view ->
      drawView { divId, suffix: show i, view } (docSel i >>> figVal) figView redraw

derive instance Newtype Paragraph _
