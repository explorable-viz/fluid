module App.View.TableView where

import Prelude

import App.Util (SelState, 𝕊(..), eventData, isTransient, selClassesFor, selected)
import App.Util.Selector (ViewSelSetter, field, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Array (filter, head, null, sort)
import Data.Maybe (fromJust)
import Data.Set (toUnfoldable)
import Dict (Dict)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Util (Endo, (!))
import Util.Map (keys, lookup)
import Val (BaseVal, Val(..), Array2)
import Web.Event.EventTarget (EventListener, eventListener)

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   -- field names of homogeneous array of records with fields of primitive type
   , colNames :: Array String
   -- values of the records. Each row has the same length as colNames
   , table :: Array2 (Val (SelState 𝕊)) -- somewhat anomalous, as elsewhere we have Selectables
   }

-- helper functions used by View.purs to decompose array of records (Dict (Val (SelState 𝕊))) into colNames and table
headers :: Array (Dict (Val (SelState 𝕊))) -> Array String
headers records = sort <<< toUnfoldable <<< keys <<< unsafePartial fromJust $ head records

arrayDictToArray2 :: forall a. Array String -> Array (Dict a) -> Array2 a
arrayDictToArray2 colNames = map (dictToArray colNames)
   where
   dictToArray keys d = map (\k -> unsafePartial fromJust $ lookup k d) keys

isCellTransient :: Array2 (Val (SelState 𝕊)) -> Int -> Int -> Boolean
isCellTransient table i j = isTransient <<< tableViewHelpers.val_selState $ table ! i ! j

foreign import drawTable :: TableViewHelpers -> EventListener -> Renderer TableView

type TableViewHelpers =
   { rowKey :: String
   , record_isUsed :: Array (Val (SelState 𝕊)) -> Boolean
   , cell_selClassesFor :: String -> SelState 𝕊 -> String
   -- values in table cells are not "unpacked" to Selectable but remain as Val
   , val_val :: Val (SelState 𝕊) -> BaseVal (SelState 𝕊)
   , val_selState :: Val (SelState 𝕊) -> SelState 𝕊
   }

tableViewHelpers :: TableViewHelpers
tableViewHelpers =
   { rowKey
   , record_isUsed
   , cell_selClassesFor
   , val_val: \(Val _ v) -> v
   , val_selState: \(Val α _) -> α
   }
   where
   rowKey :: String
   rowKey = "__n"

   record_isUsed :: Array (Val (SelState 𝕊)) -> Boolean
   record_isUsed r = not <<< null $ flip filter r \v -> selected $ (_ /= None) <$> (v # \(Val α _) -> α)

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
      tableViewSelSetter { i, colName } = listElement i <<< field colName

      filterToggleListener :: FilterToggler -> Effect EventListener
      filterToggleListener toggler = eventListener (eventData >>> toggler >>> (\_ -> identity) >>> redraw)

-- convert mouse event data (here, always rowKey) to view change
type FilterToggler = String -> Endo TableView

filterToggler :: FilterToggler
filterToggler _ (TableView view) = TableView view { filter = not view.filter }

-- 0-based index of selected record and name of field; see data binding in .js (-1th field name is __n, the rowKey)
type CellIndex = { i :: Int, colName :: String }
