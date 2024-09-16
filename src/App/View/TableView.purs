module App.View.TableView where

import Prelude

import App.Util (SelState, 𝕊(..), eventData, getPersistent, getTransient, isInert, isTransient, selClassesFor)
import App.Util.Selector (ViewSelSetter, field, listElement)
import App.View.Util (class Drawable, Renderer, selListener, uiHelpers)
import Data.Array (filter, head, length, null, sort)
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable)
import Dict (Dict)
import Effect (Effect)
import Util (Endo, definitely', (!))
import Util.Map (get, keys)
import Val (BaseVal, Val(..), Array2)
import Web.Event.EventTarget (EventListener, eventListener)

type RecordRow = Array (Val (SelState 𝕊)) -- somewhat anomalous, as elsewhere we have Selectables

data Filter = Everything | Interactive | Relevant -- TODO: rename to Filter

newtype TableView = TableView
   { title :: String
   , filter :: Filter
   , colNames :: Array String
   -- homogeneous array of records with fields of primitive type; each row has same length as colNames
   , table :: Array RecordRow
   }

-- helper functions used by View.purs to decompose array of records (Dict (Val (SelState 𝕊))) into colNames and table
headers :: Array (Dict (Val (SelState 𝕊))) -> Array String
headers records = sort <<< toUnfoldable <<< keys <<< definitely' $ head records

arrayDictToArray2 :: forall a. Array String -> Array (Dict a) -> Array2 a
arrayDictToArray2 = map <<< flip (map <<< flip get)

foreign import drawTable :: TableViewHelpers -> EventListener -> Renderer TableView

type TableViewHelpers =
   { rowKey :: String
   , record_isDisplayable :: Array (Val (SelState 𝕊)) -> Boolean
   , cell_selClassesFor :: String -> SelState 𝕊 -> String
   -- values in table cells are not "unpacked" to Selectable but remain as Val
   , val_val :: Val (SelState 𝕊) -> BaseVal (SelState 𝕊)
   , val_selState :: Val (SelState 𝕊) -> SelState 𝕊
   , hasRightBorder :: Array RecordRow -> Int -> Int -> Boolean
   , hasBottomBorder :: Array RecordRow -> Int -> Int -> Boolean
   }

defaultFilter :: Filter
defaultFilter = Interactive

tableViewHelpers :: TableViewHelpers
tableViewHelpers =
   { rowKey
   , record_isDisplayable
   , cell_selClassesFor
   , val_val
   , val_selState
   , hasRightBorder
   , hasBottomBorder
   }
   where
   rowKey = "__n"
   val_val (Val _ v) = v
   val_selState (Val α _) = α

   width :: Array RecordRow -> Int
   width table = length <<< definitely' $ head table

   record_isDisplayable :: Array (Val (SelState 𝕊)) -> Boolean
   record_isDisplayable r =
      not <<< null $ flip filter r \(Val α _) -> outFind defaultFilter α
      where
      outFind :: Filter -> SelState 𝕊 -> Boolean
      outFind Everything = const true
      outFind Interactive = not isInert
      outFind Relevant = not (isNone || isInert)

      isNone :: SelState 𝕊 -> Boolean
      isNone a = getPersistent a == None && getTransient a == None

   cell_selClassesFor :: String -> SelState 𝕊 -> String
   cell_selClassesFor colName s
      | colName == rowKey = ""
      | otherwise = selClassesFor s

   prevVisibleRow :: Array RecordRow -> Int -> Maybe Int
   prevVisibleRow table this
      | this <= 0 = Nothing
      | record_isDisplayable $ table ! (this - 1) = Just (this - 1)
      | otherwise = prevVisibleRow table (this - 1)

   nextVisibleRow :: Array RecordRow -> Int -> Maybe Int
   nextVisibleRow table this
      | this == length table - 1 = Nothing
      | record_isDisplayable $ table ! (this + 1) = Just (this + 1)
      | otherwise = nextVisibleRow table (this + 1)
{-
   cellShadowStyles :: Array RecordRow -> Int -> Int -> String
   cellShadowStyles table i j = combineStyles $ map (isCellTransient table i j && _)
      [ isNothing prev || not (isCellTransient table (definitely' prev) j)
      , j == width table - 1 || not (isCellTransient table i (j + 1))
      , isNothing next || not (isCellTransient table (definitely' next) j)
      , j == -1 || not (isCellTransient table i (j - 1))
      ]
      where
      prev = prevVisibleRow table i
      next = nextVisibleRow table i

      combineStyles :: Array Boolean -> String
      combineStyles [ false, false, false, false ] = "box-shadow: none;"
      combineStyles dirs =
         "box-shadow:" <> (joinWith ", " <<< map snd <<< filter fst $ zip dirs shadowStyles) <> ";"
         where
         shadowStyles =
            [ "inset 0px 1px 1px rgba(0, 0, 255, 0.3)" -- top
            , "inset -1px 0 1px rgba(0, 0, 255, 0.3)" -- right
            , "inset 0 -1px 1px rgba(0, 0, 255, 0.3)" -- bottom
            , "inset 1px 0 1px rgba(0, 0, 255, 0.3)" -- left
            ]
-}
   hasRightBorder :: Array RecordRow -> Int -> Int -> Boolean
   hasRightBorder table i j
      | j == width table - 1 = isCellTransient table i j
      | otherwise = isCellTransient table i j /= isCellTransient table i (j + 1)

   hasBottomBorder :: Array RecordRow -> Int -> Int -> Boolean
   hasBottomBorder table i j
      | i /= -1 && (not <<< tableViewHelpers.record_isDisplayable $ table ! i) = false -- change this
      | otherwise = case nextVisibleRow table i of
           Nothing -> isCellTransient table i j
           Just next -> isCellTransient table i j /= isCellTransient table next j

-- If I try to make this local to tableViewHelpers something goes wrong, can't see why..
isCellTransient :: Array RecordRow -> Int -> Int -> Boolean
isCellTransient table i j
   | i == -1 || j == -1 = false -- header row has j = -1 and rowKey column has i = -1
   | otherwise = isTransient <<< tableViewHelpers.val_selState $ table ! i ! j

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
filterToggler _ (TableView view) = TableView view { filter = rot view.filter }
   where
   rot :: Endo Filter
   rot Everything = Interactive
   rot Interactive = Relevant
   rot Relevant = Everything

-- 0-based index of selected record and name of field; see data binding in .js (-1th field name is __n, the rowKey)
type CellIndex = { i :: Int, colName :: String }
