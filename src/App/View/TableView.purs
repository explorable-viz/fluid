module App.View.TableView where

import Prelude hiding (absurd)

import App.Util (SelState, 𝕊(..), classes, getPersistent, getTransient, isInert, isTransient, selClasses, selClassesFor)
import App.Util.Selector (ViewSelSetter, dictVal, listElement)
import App.View.Util (class Drawable, class Drawable2, draw', registerMouseListeners, selListener, uiHelpers)
import App.View.Util.D3 (ElementType(..), classed, create, datum, select, selectAll, setDatum, setStyles, setText)
import App.View.Util.D3 as D3
import Bind ((↦))
import Data.Array (filter, head, null, partition, sort)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Set (toUnfoldable)
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Dict (Dict)
import Effect (Effect)
import Util (Endo, type (×), (×), absurd, definitely', error, length, (!))
import Util.Map (get, keys)
import Val (Array2, BaseVal(..), Val(..))
import Web.Event.EventTarget (EventListener)

type Record' = Array (Val (SelState 𝕊)) -- somewhat anomalous, as elsewhere we have Selectables

data Filter = Everything | Interactive | Relevant

-- homogeneous array of records with fields of primitive type; each row has same length as colNames
newtype TableView = TableView
   { title :: String
   , filter :: Filter
   , colNames :: Array String
   , rows :: Array Record' -- would list make more sense given the filtering?
   }

-- helpers to decompose array of records represented as dictionaries into colNames and rows
headers :: Array (Dict (SelState 𝕊 × Val (SelState 𝕊))) -> Array String
headers records = sort <<< toUnfoldable <<< keys <<< definitely' $ head records

arrayDictToArray2 :: forall a. Array String -> Array (Dict a) -> Array2 a
arrayDictToArray2 = map <<< flip (map <<< flip get)

defaultFilter :: Filter
defaultFilter = Interactive

rowKey :: String
rowKey = "__n"

cell_selClassesFor :: String -> SelState 𝕊 -> String
cell_selClassesFor colName s
   | colName == rowKey = ""
   | otherwise = selClassesFor s

record_isVisible :: Record' -> Boolean
record_isVisible r =
   not <<< null $ flip filter r \(Val α _) -> visible defaultFilter α
   where
   visible :: Filter -> SelState 𝕊 -> Boolean
   visible Everything = const true
   visible Interactive = not isInert
   visible Relevant = not (isNone || isInert)

   isNone :: SelState 𝕊 -> Boolean
   isNone a = getPersistent a == None && getTransient a == None

prim :: Val (SelState 𝕊) -> String
prim (Val _ v) = v # case _ of
   Int n -> show n
   Float n -> toStringWith (fixed 2) n
   Str s -> s
   _ -> error $ "TableView only supports primitive values."

transparentBorder :: String
transparentBorder = "1px solid transparent"

solidBorder :: String
solidBorder = "1px solid blue"

setSelState :: TableView -> EventListener -> D3.Selection -> Effect Unit
setSelState (TableView { title, rows }) redraw rootElement = do
   cells <- rootElement # selectAll ".table-cell"
   for_ cells \cell -> do
      { i, j, colName } :: CellIndex <- datum cell
      if i == -1 || j == -1 then pure unit
      else cell # classed selClasses false
         >>= classed (cell_selClassesFor colName (rows ! i ! j # \(Val α _) -> α)) true
         >>= registerMouseListeners redraw
      cell # setStyles
         [ "border-right" ↦ border (hasRightBorder i j) (j == width - 1)
         , "border-bottom" ↦ border (hasBottomBorder i j) (i == length rows - 1)
         ]
   hideRecords >>= setCaption
   where
   hideRecords :: Effect Int
   hideRecords = do
      rows' <- rootElement # selectAll ".table-row"
      { no: hidden, yes: visible } <- partition snd <$> for rows' \row -> do
         { i } <- datum row
         pure (row × record_isVisible (rows ! i))
      for_ hidden $ fst >>> classed "hidden" true
      for_ visible $ fst >>> classed "hidden" false
      pure (length hidden)

   setCaption :: Int -> Effect Unit
   setCaption numHidden = do
      let caption = title <> " (" <> show (length rows - numHidden) <> " of " <> show (length rows) <> ")"
      void $ rootElement # select ".table-caption" >>= setText caption

   width :: Int
   width = length (definitely' (head rows))

   visibleSucc :: Int -> Maybe Int
   visibleSucc i
      | i == length rows - 1 = Nothing
      | record_isVisible $ rows ! (i + 1) = Just (i + 1)
      | otherwise = visibleSucc (i + 1)

   -- For a non-header (>=0) row, the immediately prior visible row (potentially the header)
   visiblePred :: Int -> Int
   visiblePred i
      | i < 0 = error absurd
      | i == 0 = -1
      | record_isVisible (rows ! (i - 1)) = i - 1
      | otherwise = visiblePred (i - 1)

   border :: Boolean -> Boolean -> String
   border true _ = solidBorder
   border false true = transparentBorder
   border false false = ""

   hasRightBorder :: Int -> Int -> Boolean
   hasRightBorder i j
      | j == width - 1 = isCellTransient i j
      | otherwise = isCellTransient i j /= isCellTransient i (j + 1)

   hasBottomBorder :: Int -> Int -> Boolean
   hasBottomBorder i j =
      case visibleSucc i of
         Nothing -> isCellTransient i j
         Just i' -> (isCellTransient i' j /= isCellTransient (visiblePred i') j) && i == i' - 1

   isCellTransient :: Int -> Int -> Boolean
   isCellTransient i j
      | i == -1 || j == -1 = false
      | otherwise = isTransient <<< (\(Val α _) -> α) $ rows ! i ! j

createRootElement :: TableView -> D3.Selection -> String -> Effect D3.Selection
createRootElement (TableView { colNames, filter, rows }) div childId = do
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
      row' <- body # create TR [ classes [ "table-row" ] ] >>= setDatum { i }
      forWithIndex_ ([ show (i + 1) ] <> (row <#> prim)) \j value -> do
         row' # create TD [ classes if j >= 0 then [ "table-cell" ] else [] ]
            >>= setStyles [ "border-top" ↦ transparentBorder, "border-left" ↦ transparentBorder ]
            >>= setText value
            >>= setDatum { i, j: j - 1, value, colName: colNames' ! j } -- TODO: rename "value" to "text"?
   pure rootElement
   where
   createHeader colNames' rootElement = do
      row <- rootElement # create THead [] >>= create TR []
      forWithIndex_ colNames' \j colName -> do
         let value = if colName == rowKey then if filter == Relevant then "▸" else "▾" else colName
         row
            # create TH [ classes ([ "table-cell" ] <> cellClasses colName) ]
            >>= setText value
            >>= setDatum { i: -1, j: j - 1, value, colName: colNames' ! j }

   cellClasses colName
      | colName == rowKey = [ "filter-toggle", "toggle-button" ]
      | otherwise = []

instance Drawable2 TableView where
   createRootElement = createRootElement
   setSelState = setSelState

instance Drawable TableView where
   draw rSpec figVal _ redraw = do
      draw' uiHelpers rSpec =<< selListener figVal redraw tableViewSelSetter
      where
      tableViewSelSetter :: ViewSelSetter CellIndex
      tableViewSelSetter { i, colName } = listElement i <<< dictVal colName

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

-- 0-based index of selected record and name of field; -1th field name is "__n" (rowKey)
type CellIndex = { i :: Int, j :: Int, colName :: String, value :: String }

-- ======================
-- boilerplate
-- ======================
derive instance Eq Filter
