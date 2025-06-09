module App.View.Paragraph where

import Prelude

import App.Util.Selector (docSel)
import App.View.Util (class Drawable, class Drawable2, View, Select, createRootElement, draw', selListener', setSelStates, uiHelpers, unpack)
import App.View.Util.D3 (create, ElementType(..))
import App.View.Util.D3 as D3
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import Data.Newtype (class Newtype)
import Effect (Effect)

newtype Paragraph = Paragraph (Array View)

instance Drawable Paragraph where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec (selListener' figVal redraw)

instance Drawable2 Paragraph where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: Paragraph -> D3.Selection -> Effect D3.Selection
createRootElement' (Paragraph views) div = do
   rootElement <- div # create G []
   sequence_ $ flip mapWithIndex views \_ view -> do
      unpack view \v -> createRootElement v rootElement
   pure rootElement

setSelStates' :: Paragraph -> Select -> D3.Selection -> Effect Unit
setSelStates' (Paragraph views) redraw rootElement = do
   sequence_ $ flip mapWithIndex views \i view -> do
      unpack view \v -> setSelStates v (redraw <<< docSel i) rootElement

derive instance Newtype Paragraph _
