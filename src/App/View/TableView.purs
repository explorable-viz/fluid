module App.View.TableView where

import Prelude

import App.Util (Handler, Renderer, SelState, 𝕊, eventData)
import App.Util.Selector (field, listElement)
import Data.Tuple (uncurry)
import Dict (Dict)
import Val (Val)

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   , table :: Array (Dict (Val (SelState 𝕊))) -- somewhat anomalous as elsewhere we have Selectables
   }

foreign import drawTable :: Renderer TableView

-- 1-based index of selected record and name of field; see data binding in .js (0th field name is indexKey)
type CellIndex = { __n :: Int, name :: String }

tableViewHandler :: Handler
tableViewHandler = eventData >>> uncurry \{ __n, name } -> listElement (__n - 1) <<< field name
