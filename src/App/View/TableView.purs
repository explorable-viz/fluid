module App.View.TableView where

import Prelude

import App.Util (SelState, ViewSelector, 𝕊(..), eventData, selClassesFor, selected)
import App.Util.Selector (field, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Dict (Dict)
import Effect (Effect)
import Util (Endo, spy)
import Util.Map (filterKeys, get)
import Util.Set (isEmpty)
import Val (BaseVal, Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

newtype TableView = TableView
   { title :: String
   -- homogeneous array of records with fields of primitive type
   , table :: Array (Dict (Val (SelState 𝕊))) -- somewhat anomalous, as elsewhere we have Selectables
   }

type TableViewState =
   { filter :: Boolean
   }

type TableViewHelpers =
   { rowKey :: String
   , record_isUsed :: Dict (Val (SelState 𝕊)) -> Boolean
   , cell_selClassesFor :: String -> SelState 𝕊 -> String
   -- values in table cells are not "unpacked" to Selectable but remain as Val
   , val_val :: Val (SelState 𝕊) -> BaseVal (SelState 𝕊)
   , val_selState :: Val (SelState 𝕊) -> SelState 𝕊
   }

foreign import drawTable :: TableViewHelpers -> EventListener -> Renderer TableView TableViewState

drawTable' :: EventListener -> Renderer TableView TableViewState
drawTable' = drawTable
   { rowKey
   , record_isUsed
   , cell_selClassesFor
   , val_val: \(Val _ v) -> v
   , val_selState: \(Val α _) -> α
   }

instance Drawable TableView TableViewState where
   draw redraw rspec = do
      toggleListener <- filterToggleListener filterToggler
      drawTable' toggleListener uiHelpers rspec =<< selListener redraw tableViewSelector
      where
      tableViewSelector :: ViewSelector CellIndex
      tableViewSelector { __n, colName } = listElement (__n - 1) <<< field colName

      filterToggleListener :: FilterToggler -> Effect EventListener
      filterToggleListener toggler =
         eventListener (eventData >>> toggler >>> (\_ -> spy "TODO" identity) >>> redraw)

-- convert mouse event data (here, always rowKey) to view change
type FilterToggler = String -> Endo TableViewState

filterToggler :: FilterToggler
filterToggler _ vw = vw { filter = not vw.filter }

-- 1-based index of selected record and name of field; see data binding in .js (0th field name is rowKey)
type CellIndex = { __n :: Int, colName :: String }

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
