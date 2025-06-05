module App.View.Paragraph where

import Prelude

import App.Util.Selector (docSel)
import App.View.Util (class Drawable, class Drawable2, View, createRootElement, draw', selListener, setSelStates, uiHelpers, unpack)
import App.View.Util.D3 (create, ElementType(..))
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Web.Event.EventTarget (EventListener)

newtype Paragraph = Paragraph (Array View)

instance Drawable Paragraph where
   draw rSpec figVal _ redraw =
      draw' uiHelpers rSpec (selListener figVal redraw docSel)

instance Drawable2 Paragraph where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: Paragraph -> D3.Selection -> String -> Effect D3.Selection
createRootElement' (Paragraph views) div childId = do
   rootElement <- div # create G [ "id" ↦ childId ]
   sequence_ $ flip mapWithIndex views \i view -> do
      unpack view \v -> createRootElement v rootElement (childId <> "-" <> show i)
   pure rootElement

setSelStates' :: Paragraph -> EventListener -> D3.Selection -> Effect Unit
setSelStates' (Paragraph views) redraw rootElement = do
   sequence_ $ flip map views \view -> do
      unpack view \v -> setSelStates v redraw rootElement

derive instance Newtype Paragraph _
