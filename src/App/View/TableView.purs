module App.View.TableView where

import Prelude

import App.Util (SelState, ViewSelector, 𝕊(..), selClassesFor, selected)
import App.Util.Selector (field, listElement)
import App.View.Util (Renderer)
import Dict (Dict)
import Util.Map (filterKeys, get)
import Util.Set (isEmpty)
import Val (Val(..))

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   -- homogeneous array of records with fields of primitive type
   , table :: Array (Dict (Val (SelState 𝕊))) -- somewhat anomalous, as elsewhere we have Selectables
   }

foreign import drawTable :: Renderer TableView

-- 1-based index of selected record and name of field; see data binding in .js (0th field name is rowKey)
type CellIndex = { __n :: Int, colName :: String }

tableViewSelector :: ViewSelector CellIndex
tableViewSelector { __n, colName } = listElement (__n - 1) <<< field colName

rowKey :: String
rowKey = "__n"

-- Defined for any record type with fields of primitive type
record_isUsed :: Dict (Val (SelState 𝕊)) -> Boolean
record_isUsed r =
   not <<< isEmpty $ flip filterKeys r \k ->
      k /= rowKey && selected (not <<< (_ == None) <$> (get k r # \(Val α _) -> α))

cell_selClassesFor :: String -> SelState 𝕊 -> String
cell_selClassesFor colName s
   | colName == rowKey = ""
   | otherwise = selClassesFor s
