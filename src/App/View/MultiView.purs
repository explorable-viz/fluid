module App.View.MultiView where

import Prelude

import App.Util.Selector (multiViewEntry)
import App.View.Util (class Drawable, class Drawable2, View, drawView)
import App.View.Util.D3 (ElementType(..), create)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Foldable (sequence_)
import Data.Newtype (class Newtype)
import Dict (Dict)
import Effect (Effect)
import Util (error)
import Util.Map (mapWithKey)
import Web.Event.EventTarget (EventListener)

newtype MultiView = MultiView (Dict View)

instance Drawable MultiView where
   draw { divId, view: MultiView views } figVal figView redraw =
      sequence_ $ flip mapWithKey views \x view ->
         drawView { divId, suffix: x, view } (multiViewEntry x >>> figVal) figView redraw

instance Drawable2 MultiView where
   createRootElement = createRootElement'
   setSelStates = setSelStates

createRootElement' :: MultiView -> D3.Selection -> String -> Effect D3.Selection
createRootElement' (MultiView views) div childId = do
   _ <- div # create G [ "id" ↦ childId ]
   sequence_ $ flip mapWithKey views \_ _ -> do
      error "todo"
   error "todo"

setSelStates :: MultiView -> EventListener -> D3.Selection -> Effect Unit
setSelStates _ = error "todo"

derive instance Newtype MultiView _
