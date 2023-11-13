module App.TableView where

import Prelude

import App.Util (Handler, Renderer)
import Dict (Dict)
import Lattice (𝔹)
import Val (Val)


newtype Table r = Table { title :: String, table :: Array r }

foreign import drawTable :: Renderer (Table (Dict (Val 𝔹)))

tableViewHandler :: Handler
tableViewHandler = const identity
