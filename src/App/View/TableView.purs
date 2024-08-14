module App.View.TableView
   ( CellIndex
   , FilterToggler
   , TableView(..)
   , TableViewHelpers
   , TableViewState
   , drawTable
   , drawTable'
   , filterToggler
   , rowKey
   , rrecord_isReactive
   , rrecord_isUsed
   ) where

import Prelude

import App.Util (ReactState, ViewSelector, 𝕊, eventData, selClassesFor, isInert, isNone)
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
   , table :: Array (Dict (Val (ReactState 𝕊))) -- somewhat anomalous, as elsewhere we have Selectables
   }

type TableViewState =
   { filter :: Boolean
   -- this is where we'd add in UI to make this filter (3x3) or smth.
   }

type TableViewHelpers =
   { rowKey :: String
   , rrecord_isUsed :: Dict (Val (ReactState 𝕊)) -> Boolean
   , rrecord_isReactive :: Dict (Val (ReactState 𝕊)) -> Boolean
   , cell_selClassesFor :: String -> ReactState 𝕊 -> String
   -- values in table cells are not "unpacked" to Selectable but remain as Val
   , val_val :: Val (ReactState 𝕊) -> BaseVal (ReactState 𝕊)
   , val_selState :: Val (ReactState 𝕊) -> ReactState 𝕊
   }

foreign import drawTable :: TableViewHelpers -> EventListener -> Renderer TableView TableViewState

drawTable' :: EventListener -> Renderer TableView TableViewState
drawTable' = drawTable
   { rowKey
   , rrecord_isUsed
   , rrecord_isReactive
   , cell_selClassesFor
   , val_val: \(Val _ v) -> v
   , val_selState: \(Val α _) -> α
   }

instance Drawable TableView TableViewState where
   draw divId suffix redraw view viewState = do
      toggleListener <- filterToggleListener filterToggler
      drawTable' toggleListener { uiHelpers, divId, suffix, view, viewState } =<< selListener redraw tableViewSelector
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

rrecord_isUsed :: Dict (Val (ReactState 𝕊)) -> Boolean
rrecord_isUsed r =
   not <<< isEmpty $ flip filterKeys r \k ->
      k /= rowKey && (not isNone (get k r # \(Val α _) -> α))

rrecord_isReactive :: Dict (Val (ReactState 𝕊)) -> Boolean
rrecord_isReactive r =
   not <<< isEmpty $ flip filterKeys r \k ->
      (k /= rowKey) && (not isInert (get k r # \(Val α _) -> α))

cell_selClassesFor :: String -> ReactState 𝕊 -> String
cell_selClassesFor colName s
   | colName == rowKey = ""
   | otherwise = selClassesFor s
