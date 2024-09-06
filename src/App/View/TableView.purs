module App.View.TableView where

import Prelude

import App.Util (SelState, 𝕊(..), eventData, isTransient, selClassesFor, selected)
import App.Util.Selector (ViewSelSetter, field, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Array (filter, head, length, null, sort, zip)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Set (toUnfoldable)
import Data.String (joinWith)
import Data.Tuple (fst, snd)
import Dict (Dict)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Util (Endo, (!))
import Util.Map (keys, lookup)
import Val (BaseVal, Val(..), Array2)
import Web.Event.EventTarget (EventListener, eventListener)

type Table = Array2 (Val (SelState 𝕊))

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   -- field names of homogeneous array of records with fields of primitive type
   , colNames :: Array String
   -- values of the records. Each row has the same length as colNames
   , table :: Table -- somewhat anomalous, as elsewhere we have Selectables
   }

-- helper functions used by View.purs to decompose array of records (Dict (Val (SelState 𝕊))) into colNames and table
headers :: Array (Dict (Val (SelState 𝕊))) -> Array String
headers records = sort <<< toUnfoldable <<< keys <<< unsafePartial fromJust $ head records

arrayDictToArray2 :: forall a. Array String -> Array (Dict a) -> Array2 a
arrayDictToArray2 colNames = map (dictToArray colNames)
   where
   dictToArray keys d = map (\k -> unsafePartial fromJust $ lookup k d) keys

width :: Table -> Int
width table = length <<< unsafePartial fromJust $ head table

isCellTransient :: Table -> Int -> Int -> Boolean
isCellTransient table i j
   | i == -1 || j == -1 = false -- header row now has j = -1 and rowKey column now has i = -1
   | otherwise = isTransient <<< tableViewHelpers.val_selState $ table ! i ! j

hasRightBorder :: Table -> Int -> Int -> Boolean
hasRightBorder table i j
   | j == width table - 1 = isCellTransient table i j
   | otherwise = isCellTransient table i j /= isCellTransient table i (j + 1)

hasBottomBorder :: Table -> Int -> Int -> Boolean
hasBottomBorder table i j
   | i /= -1 && (not <<< tableViewHelpers.record_isUsed $ table ! i) = false -- change this
   | otherwise = case nextVisibleRow table i of
        Nothing -> isCellTransient table i j
        Just next -> isCellTransient table i j /= isCellTransient table next j

prevVisibleRow :: Table -> Int -> Maybe Int
prevVisibleRow table this
   | this <= 0 = Nothing
   | tableViewHelpers.record_isUsed $ table ! (this - 1) = Just (this - 1)
   | otherwise = prevVisibleRow table (this - 1)

nextVisibleRow :: Table -> Int -> Maybe Int
nextVisibleRow table this
   | this == length table - 1 = Nothing
   | tableViewHelpers.record_isUsed $ table ! (this + 1) = Just (this + 1)
   | otherwise = nextVisibleRow table (this + 1)

cellShadowStyles :: Table -> Int -> Int -> String
cellShadowStyles table i j = combineStyles $ map (isCellTransient table i j && _)
   [ isNothing prev || not (isCellTransient table (unsafePartial fromJust prev) j)
   , j == width table - 1 || not (isCellTransient table i (j + 1))
   , isNothing next || not (isCellTransient table (unsafePartial fromJust next) j)
   , j == -1 || not (isCellTransient table i (j - 1))
   ]
   where
   prev = prevVisibleRow table i
   next = nextVisibleRow table i

combineStyles :: Array Boolean -> String
combineStyles [ false, false, false, false ] = "box-shadow: none;"
combineStyles dirs = "box-shadow:" <> (joinWith ", " <<< map snd <<< filter fst $ zip dirs shadowStyles) <> ";"
   where
   shadowStyles =
      [ "inset 0px 1px 1px rgba(0, 0, 255, 0.3)" -- top
      , "inset -1px 0 1px rgba(0, 0, 255, 0.3)" -- right
      , "inset 0 -1px 1px rgba(0, 0, 255, 0.3)" -- bottom
      , "inset 1px 0 1px rgba(0, 0, 255, 0.3)" -- left
      ]

foreign import drawTable :: TableViewHelpers -> EventListener -> Renderer TableView

type TableViewHelpers =
   { rowKey :: String
   , record_isUsed :: Array (Val (SelState 𝕊)) -> Boolean
   , cell_selClassesFor :: String -> SelState 𝕊 -> String
   -- values in table cells are not "unpacked" to Selectable but remain as Val
   , val_val :: Val (SelState 𝕊) -> BaseVal (SelState 𝕊)
   , val_selState :: Val (SelState 𝕊) -> SelState 𝕊
   , hasRightBorder :: Table -> Int -> Int -> Boolean
   , hasBottomBorder :: Table -> Int -> Int -> Boolean
   , cellShadowStyles :: Table -> Int -> Int -> String
   }

tableViewHelpers :: TableViewHelpers
tableViewHelpers =
   { rowKey
   , record_isUsed
   , cell_selClassesFor
   , val_val: \(Val _ v) -> v
   , val_selState: \(Val α _) -> α
   , hasRightBorder
   , hasBottomBorder
   , cellShadowStyles
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
