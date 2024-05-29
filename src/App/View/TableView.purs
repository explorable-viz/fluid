module App.View.TableView where

import Prelude

import App.Util (Renderer, SelState, 𝕊, ViewSelector)
import App.Util.Selector (field, listElement)
import Dict (Dict)
import Val (Val)

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   , table :: Array (Dict (Val (SelState 𝕊))) -- somewhat anomalous as elsewhere we have Selectables
   }

foreign import drawTable :: Renderer TableView

-- 1-based index of selected record and name of field; see data binding in .js (0th field name is rowKey)
type CellIndex = { __n :: Int, colName :: String }

tableViewSelector :: ViewSelector CellIndex
tableViewSelector { __n, colName } = listElement (__n - 1) <<< field colName
