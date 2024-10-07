module App.View.Util.Axes where

import Prelude

import App.Util (Dimensions(..), classes)
import App.View.Util.D3 (Coord, ElementType(..), create, rotate, scaleLinear, selectAll, setAttrs, setStyles, translate, xAxis, yAxis)
import App.View.Util.D3 as D3
import App.View.Util.Orientation (Orientation(..))
import App.View.Util.Point (Point(..))
import Bind ((↦))
import Data.Array.NonEmpty (NonEmptyArray, nub)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Tuple (fst)
import Effect (Effect)
import Util (Endo)

class HasAxes a where
   -- all data elements to be plotted; determines axis mappings
   points :: a -> Coord (NonEmptyArray Number)
   tickLabels :: a -> Point Orientation

to :: forall a. HasAxes a => a -> Dimensions Int -> Coord (Endo Number)
to view (Dimensions range) =
   { x: scaleLinear { min: min'.x, max: max'.x } { min: 0.0, max: toNumber range.width }
   , y: scaleLinear { min: 0.0, max: max'.y } { min: toNumber range.height, max: 0.0 }
   }
   where
   { x: xs, y: ys } = points view

   min' :: Coord Number
   min' = { x: minimum xs, y: minimum ys }

   max' :: Coord Number
   max' = { x: maximum xs, y: maximum ys }

createAxes :: forall a. HasAxes a => a -> Dimensions Int -> D3.Selection -> Effect (Coord D3.Selection)
createAxes view range parent = do
   let Point { x: xLabels, y: yLabels } = tickLabels view
   x <- xAxis (to' range) (nub points'.x) =<<
      (parent # create G [ classes [ "x-axis" ], translate { x: 0, y: (unwrap range).height } ])
   when (fst xLabels == Rotated) do
      labels <- x # selectAll "text"
      for_ labels $
         setAttrs [ rotate 45 ] >=> setStyles [ "text-anchor" ↦ "start" ]
   y <- yAxis (to' range) 3.0 =<< (parent # create G [ classes [ "y-axis" ] ])
   when (fst yLabels == Rotated) do
      labels <- y # selectAll "text"
      for_ labels $
         setAttrs [ rotate 45 ] >=> setStyles [ "text-anchor" ↦ "end" ]
   pure { x, y }
   where
   to' = to view
   points' = points view
