module App.LineChart where

import Prelude
import Data.List (List(..), (:))
import Effect (Effect)
import App.Util (HTMLId, class Reflect, from, get, get_intOrNumber, get_prim, record)
import Bindings (Bind)
import DataType (cLinePlot)
import Lattice (𝔹, expand)
import Primitive (Slice)
import Util (type (×), (×))
import Util.SnocList (SnocList)
import Val (Val)
import Val (Val(..)) as V

newtype LineChart = LineChart { caption :: String × 𝔹, plots :: Array LinePlot }
newtype LinePlot = LinePlot { name :: String × 𝔹, data_ :: Array Point }
newtype Point = Point { x :: Number × 𝔹, y :: Number × 𝔹 }

foreign import drawLineChart :: HTMLId -> LineChart -> Effect Unit

instance reflectPoint :: Reflect (SnocList (Bind (Val Boolean))) Point where
   from r = Point {
      x: get_intOrNumber "x" r,
      y: get_intOrNumber "y" r
   }

instance reflectLinePlot :: Reflect (SnocList (Bind (Val Boolean))) LinePlot where
   from r = LinePlot {
      name: get_prim "name" r,
      data_: record from <$> from (get "data" r)
   }

instance reflectLineChart :: Reflect (SnocList (Bind (Val Boolean))) LineChart where
   from r = LineChart {
      caption: get_prim "caption" r,
      plots: from <$> (from (get "plots" r) :: Array (Slice (Val 𝔹))) :: Array LinePlot
   }

instance reflectLinePlot' :: Reflect (Val Boolean) LinePlot where
   from (v × V.Constr _ c (v1 : Nil)) | c == cLinePlot =
      case expand v (V.Constr false cLinePlot (V.Hole false : Nil)) of
         V.Constr _ _ (u1 : Nil) -> record from (u1 × v1)
