module App.View.BarChart2 where

import Prelude

import App.Util (Dimensions(..), Selectable, contents)
import App.Util.Selector (barChart, dictVal, listElement)
import App.View.StackedBar2 (StackedBar(..), StackedBarContext, barHeight)
import App.View.Util (Select, setSelStates2)
import App.View.Util.D3 (Margin, scaleBand, scaleLinear, selectAll, textHeight)
import App.View.Util.D3 as D3
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Data.Number (ceil)
import Data.Semigroup.Foldable (maximum)
import DataType (f_stackedBars)
import Effect (Effect)
import Util (Endo, (!))

newtype BarChart = BarChart
   { caption :: Selectable String
   , size :: Dimensions (Selectable Int)
   , stackedBars :: NonEmptyArray StackedBar
   }

setSelStates :: BarChart -> Select -> D3.Selection -> Effect Unit
setSelStates chart@(BarChart { stackedBars }) select barChart' = do
   -- more robust to iterate over stackedBars and select ith DOM child instead?
   stackedBars' <- barChart' # selectAll ".stack"
   forWithIndex_ stackedBars' \i stack ->
      setSelStates2 (stackedBarContext chart) (stackedBars ! i)
         (select <<< barChart <<< dictVal f_stackedBars <<< listElement i)
         stack

stackedBarContext :: BarChart -> StackedBarContext
stackedBarContext (BarChart { caption, size, stackedBars }) =
   { interior, scales: to interior, strokeWidth }
   where
   Dimensions { width, height } = size <#> contents

   margin :: Margin
   margin = { top: 3, right: 75, bottom: 20, left: 30 }

   interior :: Dimensions Int
   interior = Dimensions
      { width: width - margin.left - margin.right
      , height: height - margin.top - margin.bottom - caption_height
      }

   caption_class = "title-text"
   caption_height = textHeight caption_class (contents caption) * 2

   to :: Dimensions Int -> { x :: String -> Number, y :: Endo Number }
   to (Dimensions { width, height }) =
      { x: scaleBand width $ (\(StackedBar bar) -> contents bar.x) <$> toArray stackedBars
      , y: scaleLinear { min: 0.0, max: y_max } { min: toNumber height, max: 0.0 }
      }

   nearest = 10.0
   y_max = ceil $ nearest * (maximum (barHeight <$> stackedBars) / nearest)
   strokeWidth = 1
