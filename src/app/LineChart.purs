module App.LineChart where

import Prelude
import Effect (Effect)
import App.Util (HTMLId)
import Lattice (𝔹)
import Util (type (×))
import Val (Array2)

newtype LineChart = LineChart { caption :: String × 𝔹, plots :: Array LinePlot }
newtype LinePlot = LinePlot { name :: String × 𝔹, data_ :: Array Point }
newtype Point = Point { x :: Number × 𝔹, y :: Number × 𝔹 }

foreign import drawLineChart :: HTMLId -> LineChart -> Effect Unit
