module App.View.StackedBar where

import Prelude

import App.Util (PartialAttrs, Selectable, classes, contents)
import App.Util.Selector (dictVal)
import App.View.Segment (Segment(..))
import App.View.Util (class View, Select, createRootElement, setSelStates)
import App.View.Util.D3 (ElementType(..), create, selectAll)
import App.View.Util.D3 as D3
import Data.Array (scanl)
import Data.Array.NonEmpty (cons', init)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Newtype (class Newtype)
import DataType (f_segments)
import Effect (Effect)
import Util ((!))

newtype StackedBar = StackedBar
   { x :: Selectable String
   , segments :: Array Segment
   }

createRootElement'
   :: PartialAttrs { x :: String, y :: Number } { z :: Number }
   -> StackedBar
   -> D3.Selection
   -> Effect D3.Selection
createRootElement' attrFun (StackedBar { x, segments }) parent = do
   g <- parent # create G [ classes [ "stack" ] ]
   forWithIndex_ (barData <#> \y -> { x: contents x, y: y }) \j bar ->
      createRootElement (\segment attrs' _ -> attrFun bar attrs' segment) (segments ! j) g
   pure g

   where
   barData :: Array Number
   barData = init (cons' 0.0 (scanl (+) 0.0 (segments <#> \(Segment seg) -> contents seg.z)))

setSelStates' :: StackedBar -> Select -> D3.Selection -> Effect Unit
setSelStates' (StackedBar { segments }) select stackedBar = do
   segments' <- stackedBar # selectAll ".bar" -- TODO: .bar -> .segment
   forWithIndex_ segments' \j segment ->
      setSelStates (segments ! j) (select <<< dictVal f_segments) segment

instance View StackedBar { x :: String, y :: Number } { z :: Number } where
   createRootElement = createRootElement'
   setSelStates = setSelStates'

-- ======================
-- boilerplate
-- ======================

derive instance Newtype StackedBar _
