module App.TableView where

import Prelude

import App.Util (Handler, Renderer)
import Dict (Dict)
import Lattice (𝔹)
import Val (Val)

newtype TableView = TableView
   { title :: String
   , table :: Array (Dict (Val 𝔹))
   }

foreign import drawTable :: Renderer (TableView)

tableViewHandler :: Handler
tableViewHandler = const identity
