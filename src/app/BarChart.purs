module App.BarChart where

import Prelude
import Effect (Effect)
import App.Util (HTMLId)
import Lattice (𝔹)
import Util (type (×))
import Val (Array2)

newtype BarChart = BarChart { caption :: String × 𝔹, data_ :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }

foreign import drawBarChart :: HTMLId -> BarChart -> Effect Unit
