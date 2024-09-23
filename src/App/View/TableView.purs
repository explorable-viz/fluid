module App.View.TableView where

import Prelude

import App.Util (SelState, 𝕊(..), classes, getPersistent, getTransient, isInert, isTransient, selClasses, selClassesFor)
import App.Util.Selector (ViewSelSetter, field, listElement)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (ElementType(..), classed, create, datum, select, selectAll, setData, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Array (filter, head, null, sort)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (filterM, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number.Format (fixed, toStringWith)
import Data.Set (toUnfoldable)
import Dict (Dict)
import Effect (Effect)
import Util (Endo, definitely', error, length, (!))
import Util.Map (get, keys)
import Val (Array2, BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

type RecordRow = Array (Val (SelState 𝕊)) -- somewhat anomalous, as elsewhere we have Selectables

data Filter = Everything | Interactive | Relevant

newtype TableView = TableView
   { title :: String
   , filter :: Filter
   , colNames :: Array String
   -- homogeneous array of records with fields of primitive type; each row has same length as colNames
   , rows :: Array RecordRow
   }

-- helper functions used by View.purs to decompose array of records (Dict (Val (SelState 𝕊))) into colNames and table
headers :: Array (Dict (Val (SelState 𝕊))) -> Array String
headers records = sort <<< toUnfoldable <<< keys <<< definitely' $ head records

arrayDictToArray2 :: forall a. Array String -> Array (Dict a) -> Array2 a
arrayDictToArray2 = map <<< flip (map <<< flip get)

newtype TableViewHelpers = TableViewHelpers
   { rowKey :: String
   , record_isVisible :: Array (Val (SelState 𝕊)) -> Boolean
   , cell_selClassesFor :: String -> SelState 𝕊 -> String
   -- values in rows cells are not "unpacked" to Selecrows but remain as Val
   , val_val :: Val (SelState 𝕊) -> BaseVal (SelState 𝕊)
   , val_selState :: Val (SelState 𝕊) -> SelState 𝕊
   , hasRightBorder :: Array RecordRow -> Int -> Int -> Boolean
   , hasBottomBorder :: Array RecordRow -> Int -> Int -> Boolean
   }

defaultFilter :: Filter
defaultFilter = Interactive

rowKey :: String
rowKey = "__n"

cell_selClassesFor :: String -> SelState 𝕊 -> String
cell_selClassesFor colName s
   | colName == rowKey = ""
   | otherwise = selClassesFor s

record_isVisible :: RecordRow -> Boolean
record_isVisible r =
   not <<< null $ flip filter r \(Val α _) -> visible defaultFilter α
   where
   visible :: Filter -> SelState 𝕊 -> Boolean
   visible Everything = const true
   visible Interactive = not isInert
   visible Relevant = not (isNone || isInert)

   isNone :: SelState 𝕊 -> Boolean
   isNone a = getPersistent a == None && getTransient a == None

width :: Array RecordRow -> Int
width rows = length <<< definitely' $ head rows

prevVisibleRow :: Array RecordRow -> Int -> Maybe Int
prevVisibleRow rows this
   | this <= 0 = Nothing
   | record_isVisible $ rows ! (this - 1) = Just (this - 1)
   | otherwise = prevVisibleRow rows (this - 1)

nextVisibleRow :: Array RecordRow -> Int -> Maybe Int
nextVisibleRow rows this
   | this == length rows - 1 = Nothing
   | record_isVisible $ rows ! (this + 1) = Just (this + 1)
   | otherwise = nextVisibleRow rows (this + 1)

hasRightBorder :: Array RecordRow -> Int -> Int -> Boolean
hasRightBorder rows i j
   | j == width rows - 1 = isCellTransient rows i j
   | otherwise = isCellTransient rows i j /= isCellTransient rows i (j + 1)

hasBottomBorder :: Array RecordRow -> Int -> Int -> Boolean
hasBottomBorder rows i j
   | i /= -1 && (not <<< record_isVisible $ rows ! i) = false -- change this
   | otherwise = case nextVisibleRow rows i of
        Nothing -> isCellTransient rows i j
        Just next -> isCellTransient rows i j /= isCellTransient rows next j

tableViewHelpers :: TableViewHelpers
tableViewHelpers =
   TableViewHelpers
      { rowKey
      , record_isVisible
      , cell_selClassesFor
      , val_val
      , val_selState
      , hasRightBorder
      , hasBottomBorder
      }
   where
   val_val (Val _ v) = v
   val_selState (Val α _) = α

-- If I make this local to tableViewHelpers something goes wrong, can't see why..
isCellTransient :: Array RecordRow -> Int -> Int -> Boolean
isCellTransient rows i j
   | i == -1 || j == -1 = false -- header row has j = -1 and rowKey column has i = -1
   | otherwise = isTransient <<< (unwrap tableViewHelpers).val_selState $ rows ! i ! j

instance Drawable2 TableView TableViewHelpers where
   createRootElement = createRootElement
   setSelState = setSelState

prim :: Val (SelState 𝕊) -> String
prim (Val _ v) = v # case _ of
   Int n -> show n
   Float n -> toStringWith (fixed 2) n
   Str s -> s
   _ -> error $ "TableView only supports primitive values."

setSelState :: TableView -> TableViewHelpers -> EventListener -> D3.Selection -> Effect Unit
setSelState (TableView { title, rows }) _ redraw rootElement = do
   cells <- rootElement # selectAll ".table-cell"
   for_ cells \cell -> do
      { i, j, colName } :: CellIndex <- datum cell
      if i == -1 || j == -1 then pure unit
      else cell # classed selClasses false
         >>= classed (cell_selClassesFor colName (rows ! i ! j # \(Val α _) -> α)) true
         >>= registerMouseListeners redraw
      cell # classed "has-right-border" (hasRightBorder rows i j)
         >>= classed "has-bottom-border" (hasBottomBorder rows i j)
   rows' <- rootElement # selectAll ".table-row"
   hidden <- flip filterM (fromFoldable rows') \row -> do
      { i } <- datum row
      pure $ not (record_isVisible (rows ! i))
   for_ hidden $ classed "hidden" true
   let caption = title <> " (" <> show (length rows - length hidden) <> " of " <> show (length rows) <> ")"
   void $ rootElement # select ".table-caption" >>= setText caption

createRootElement :: TableView -> TableViewHelpers -> D3.Selection -> String -> Effect D3.Selection
createRootElement (TableView { colNames, filter, rows }) _ div childId = do
   rootElement <- div # create Table [ "id" ↦ childId ]
   void $ rootElement # create Caption
      [ classes [ "title-text", "table-caption" ]
      , "dominant-baseline" ↦ "middle"
      , "text-anchor" ↦ "left"
      ]
   let colNames' = [ rowKey ] <> colNames
   rootElement # createHeader colNames'
   body <- rootElement # create TBody []
   forWithIndex_ rows \i row -> do
      row' <- body # create TR [ classes [ "table-row" ] ] >>= setData { i }
      forWithIndex_ ([ show (i + 1) ] <> (row <#> prim)) \j value -> do
         cell <- row' # create TD [ classes if j >= 0 then [ "table-cell" ] else [] ]
         void $ cell
            # setStyles
                 [ "border-top" ↦ transparentBorder
                 , "border-left" ↦ transparentBorder
                 , "border-right" ↦ if j == length colNames' - 1 then transparentBorder else ""
                 , "border-bottom" ↦ if i == length rows - 1 then transparentBorder else ""
                 ]
            >>= setText value
            >>= setData { i, j: j - 1, value, colName: colNames' ! j } -- TODO: rename "value" to "text"?
   pure rootElement
   where
   transparentBorder = "1px solid transparent"

   createHeader colNames' rootElement = do
      row <- rootElement # create THead [] >>= create TR []
      forWithIndex_ colNames' \j colName -> do
         let value = if colName == rowKey then if filter == Relevant then "▸" else "▾" else colName
         row
            # create TH [ classes ([ "table-cell" ] <> cellClasses colName) ]
            >>= setText value
            >>= setData { i: -1, j: j - 1, value, colName: colNames' ! j }

   cellClasses colName
      | colName == rowKey = [ "filter-toggle", "toggle-button" ]
      | otherwise = []

instance Drawable TableView where
   draw rSpec figVal _ redraw = do
      draw' tableViewHelpers uiHelpers rSpec =<< selListener figVal redraw tableViewSelSetter
      where
      tableViewSelSetter :: ViewSelSetter CellIndex
      tableViewSelSetter { i, colName } = listElement i <<< field colName

--      toggleListener <- filterToggleListener filterToggler
--
--      filterToggleListener :: FilterToggler -> Effect EventListener
--      filterToggleListener toggler = eventListener (eventData >>> toggler >>> (\_ -> identity) >>> redraw)

-- convert mouse event data (here, always rowKey) to view change
type FilterToggler = String -> Endo TableView

filterToggler :: FilterToggler
filterToggler _ (TableView view) = TableView view { filter = rotate view.filter }
   where
   rotate :: Endo Filter
   rotate Everything = Interactive
   rotate Interactive = Relevant
   rotate Relevant = Everything

-- 0-based index of selected record and name of field; see data binding in .js (-1th field name is __n, the rowKey)
type CellIndex = { i :: Int, j :: Int, colName :: String, value :: String }

-- ======================
-- boilerplate
-- ======================
derive instance Newtype TableViewHelpers _
derive instance Eq Filter
