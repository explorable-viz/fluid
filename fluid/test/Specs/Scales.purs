module Test.Specs.Scales where

import Prelude

import App.Fig (loadFig, selectionResult)
import App.Util.Selector (none)
import App.View (reflect)
import App.View.LineChart (LineChart(..), LinePlot(..))
import App.View.Util.Point (Point(..))
import App.View.Util.Scale (Interval, Scale(..), Scales(..), resolveScale)
import Classes (class HasClasses)
import Data.Array (concatMap)
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import File (class MonadLoad, File(..), loadFile)
import Test.Util (TestSuite, fluidSrcPaths)
import Util (type (×), check, definitely', (×))
import Val (class HasModuleStore, ValWithDoc(..))

co2 :: Interval
co2 = { min: 315.0, max: 432.0 }

resolveScale_cases :: Array (String × Boolean)
resolveScale_cases =
   [ "Auto is data range" × (resolveScale Auto co2 == co2)
   , "FromZero is zero to data maximum" × (resolveScale FromZero co2 == { min: 0.0, max: 432.0 })
   , "Fixed ignores data range" × (resolveScale (Fixed 300.0 450.0) co2 == { min: 300.0, max: 450.0 })
   , "Fixed ignores other data range" × (resolveScale (Fixed 300.0 450.0) { min: 0.0, max: 10.0 } == { min: 300.0, max: 450.0 })
   ]

type FixtureSpec = { file :: String, x :: Interval, y :: Interval }

fixture_cases :: Array FixtureSpec
fixture_cases =
   [ { file: "plot/co2_fixed.fld", x: { min: 1960.0, max: 2025.0 }, y: { min: 300.0, max: 450.0 } }
   , { file: "plot/co2_auto.fld", x: { min: 1960.0, max: 2025.0 }, y: co2 }
   , { file: "plot/co2_from_zero.fld", x: { min: 1960.0, max: 2025.0 }, y: { min: 0.0, max: 432.0 } }
   ]

-- Axis domains of chart, from scales and data
domains :: LineChart -> { x :: Interval, y :: Interval }
domains (LineChart { scales: Scales scales, plots }) =
   { x: resolveScale scales.x (range _.x), y: resolveScale scales.y (range _.y) }
   where
   range proj = { min: definitely' (minimum values), max: definitely' (maximum values) }
      where
      values = concatMap (\(LinePlot { points }) -> points <#> \(Point p) -> fst (proj p)) plots

fixtureTest :: forall m. HasClasses m => HasModuleStore m => MonadLoad m => FixtureSpec -> m Unit
fixtureTest { file, x, y } = do
   fluidSrc <- loadFile fluidSrcPaths (File file)
   fig <- loadFig { fluidSrcPaths, inputs: [], query: false, ignoreInputs: none, linking: false, rowFilter: Nothing } fluidSrc
   let ValWithDoc { val } = (selectionResult fig).v
   let d = domains (reflect { rowFilter: Nothing } val)
   check (d.x == x) ("x domain " <> show d.x <> ", expected " <> show x)
   check (d.y == y) ("y domain " <> show d.y <> ", expected " <> show y)

scales_suite :: forall m. HasClasses m => HasModuleStore m => MonadLoad m => TestSuite m
scales_suite =
   (resolveScale_cases <#> \(name × ok) -> ("resolveScale: " <> name) × check ok name)
      <> (fixture_cases <#> \spec -> spec.file × fixtureTest spec)
