module App.View.TableView where

import Prelude

import App.Util (SelState, 𝕊(..), eventData, getPersistent, getTransient, isInert, selClassesFor)
import App.Util.Selector (ViewSelSetter, field, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Dict (Dict)
import Effect (Effect)
import Util (Endo)
import Util.Map (filterKeys, get)
import Util.Set (isEmpty)
import Val (BaseVal, Val(..))
import Web.Event.EventTarget (EventListener, eventListener)

data FilterType = Everything | Interactive | Relevant

newtype TableView = TableView
   { title :: String
   , filter :: FilterType
   -- homogeneous array of records with fields of primitive type
   , table :: Array (Dict (Val (SelState 𝕊))) -- somewhat anomalous, as elsewhere we have Selectables
   }

type TableViewHelpers =
   { rowKey :: String
   , record_isDisplayable :: Dict (Val (SelState 𝕊)) -> FilterType -> Boolean
   , cell_selClassesFor :: String -> SelState 𝕊 -> String
   -- values in table cells are not "unpacked" to Selectable but remain as Val
   , val_val :: Val (SelState 𝕊) -> BaseVal (SelState 𝕊)
   , val_selState :: Val (SelState 𝕊) -> SelState 𝕊
   }

foreign import drawTable :: TableViewHelpers -> EventListener -> Renderer TableView

tableViewHelpers :: TableViewHelpers
tableViewHelpers =
   { rowKey
   , record_isDisplayable
   , cell_selClassesFor
   , val_val: \(Val _ v) -> v
   , val_selState: \(Val α _) -> α
   }
   where
   rowKey :: String
   rowKey = "__n"

   --helper for "we want to display this record"
   record_isDisplayable :: Dict (Val (SelState 𝕊)) -> FilterType -> Boolean
   record_isDisplayable r filtering = 
      not <<< isEmpty $ flip filterKeys r \k ->
         k /= rowKey && not comparative (get k r # \(Val α _) -> α)
         where 
         comparative = outfind filtering
         outfind :: FilterType -> (SelState 𝕊 -> Boolean)
         outfind Everything = isThere
         outfind Interactive = isInert
         outfind Relevant = isNone || isInert

         isThere :: SelState 𝕊 -> Boolean
         isThere _ = false

         isNone :: SelState 𝕊 -> Boolean
         isNone a = getPersistent a == None && getTransient a == None

   cell_selClassesFor :: String -> SelState 𝕊 -> String
   cell_selClassesFor colName s
      | colName == rowKey = ""
      | otherwise = selClassesFor s

instance Drawable TableView where
   draw rSpec figVal _ redraw = do
      toggleListener <- filterToggleListener filterToggler
      drawTable tableViewHelpers toggleListener uiHelpers rSpec
         =<< selListener figVal redraw tableViewSelSetter
      where
      tableViewSelSetter :: ViewSelSetter CellIndex
      tableViewSelSetter { __n, colName } = listElement (__n - 1) <<< field colName

      filterToggleListener :: FilterToggler -> Effect EventListener
      filterToggleListener toggler = eventListener (eventData >>> toggler >>> (\_ -> identity) >>> redraw)

-- convert mouse event data (here, always rowKey) to view change
type FilterToggler = String -> Endo TableView

-- toggling the sides of a triangle
filterToggler :: FilterToggler
filterToggler _ (TableView view) = TableView view { filter = rot view.filter}

rot :: FilterType -> FilterType
rot Everything = Interactive
rot Interactive = Relevant
rot Relevant = Everything

-- 1-based index of selected record and name of field; see data binding in .js (0th field name is rowKey)
type CellIndex = { __n :: Int, colName :: String }
