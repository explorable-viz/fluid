module App.View.TableView where

import Prelude

import App.Util (Handler, Renderer, Sel)
import Dict (Dict)
import Val (Val)

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   , table :: Array (Dict (Val Sel))
   }

foreign import drawTable :: Renderer TableView

tableViewHandler :: Handler
tableViewHandler = const identity
