module App.Renderer where

import Prelude
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.Array (zip, zipWith)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (fst)
import Data.Profunctor.Strong (first)
import App.MatrixView (MatrixView(..), IntMatrix, drawMatrix)
import App.Util (HTMLId)
import Bindings (Bindings, Bind, Var, find)
import DataType (cBarChart, cCons, cLineChart, cLinePlot, cNil)
import Effect (Effect)
import Lattice (𝔹, expand)
import Primitive (Slice, class ToFrom, as, match, match_fwd)
import Util (type (×), (×), type (+), successful)
import Util.SnocList (SnocList)
import Val (MatrixRep, Val)
import Val (Val(..)) as V

type Fig = {
   divId :: HTMLId,
   subfigs :: Array SubFig
}

drawFig :: Fig -> Effect Unit
drawFig { divId, subfigs } =
   sequence_ $ drawSubFig divId <$> subfigs

foreign import drawBarChart :: HTMLId -> BarChart -> Effect Unit
foreign import drawLineChart :: HTMLId -> LineChart -> Effect Unit
foreign import drawTable :: HTMLId -> EnergyTable -> Effect Unit

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to specific examples for now. Matrices are assumed to have element type Int.
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
newtype BarChart = BarChart { caption :: String × 𝔹, data_ :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }
newtype EnergyTable = EnergyTable { title :: String, table :: Array EnergyRecord }
newtype LineChart = LineChart { caption :: String × 𝔹, plots :: Array LinePlot }
newtype LinePlot = LinePlot { name :: String × 𝔹, data_ :: Array Point }
newtype Point = Point { x :: Number × 𝔹, y :: Number × 𝔹 }

data SubFig =
   MatrixFig MatrixView |
   EnergyTableView EnergyTable |
   LineChartFig LineChart |
   BarChartFig BarChart

drawSubFig :: HTMLId -> SubFig -> Effect Unit
drawSubFig divId (MatrixFig fig) = drawMatrix divId fig
drawSubFig divId (EnergyTableView fig) = drawTable divId fig
drawSubFig divId (LineChartFig fig) = drawLineChart divId fig
drawSubFig divId (BarChartFig fig) = drawBarChart divId fig

-- Convert sliced value to appropriate SubFig, discarding top-level annotations for now.
makeSubFig :: Partial => { title :: String, uv :: Slice (Val 𝔹) } -> SubFig
makeSubFig { title, uv: u × V.Constr _ c (v1 : Nil) } | c == cBarChart =
   case expand u (V.Constr false cBarChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) -> BarChartFig (record from (u1 × v1))
makeSubFig { title, uv: u × V.Constr _ c (v1 : Nil) } | c == cLineChart =
   case expand u (V.Constr false cLineChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) -> LineChartFig (record from (u1 × v1))
makeSubFig { title, uv: u × v@(V.Constr _ c _) } | c == cNil || c == cCons =
   EnergyTableView (EnergyTable { title, table: record energyRecord <$> from (u × v) })
makeSubFig { title, uv: u × v@(V.Matrix _ _) } =
   let vss2 = fst (match_fwd (u × v)) × fst (match v) in
   MatrixFig (MatrixView { title, matrix: matrixRep vss2 } )

-- Assumes fields are all of primitive type.
record :: forall a . (Slice (Bindings (Val 𝔹)) -> a) -> Slice (Val 𝔹) -> a
record toRecord (u × v) = toRecord (fst (match_fwd (u × v)) × fst (match v))

energyRecord :: Slice (Bindings (Val 𝔹)) -> EnergyRecord
energyRecord r = {
   year: get_prim "year" r,
   country: get_prim "country" r,
   energyType: get_prim "energyType" r,
   output: get_intOrNumber "output" r
}

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) (zipWith zip vss uss) × i × j

get_prim :: forall a . ToFrom a => Var -> Slice (Bindings (Val 𝔹)) -> a × 𝔹
get_prim x = match_fwd <<< get x

get_intOrNumber :: Var -> Slice (Bindings (Val 𝔹)) -> Number × 𝔹
get_intOrNumber x r = first as (get_prim x r :: (Int + Number) × 𝔹)

get :: Var -> Slice (Bindings (Val 𝔹)) -> Slice (Val 𝔹)
get x (r × r') = successful $ find x r `lift2 (×)` find x r'

class Reflect a b where
   from :: Partial => Slice a -> b

instance reflectBarChartRecord :: Reflect (SnocList (Bind (Val Boolean))) BarChartRecord where
   from r = BarChartRecord {
      x: get_prim "x" r,
      y: get_intOrNumber "y" r
   }

instance reflectBarChart :: Reflect (SnocList (Bind (Val Boolean))) BarChart where
   from r = BarChart {
      caption: get_prim "caption" r,
      data_: record from <$> from (get "data" r)
   }

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

-- Perform hole expansion as necessary, and discard any constructor-level annotations.
instance reflectArray :: Reflect (Val Boolean) (Array (Val Boolean × Val Boolean)) where
   from (vs × V.Constr _ c Nil) | c == cNil =
      case expand vs (V.Constr false cNil Nil) of
         V.Constr _ _ Nil -> []
   from (us × V.Constr _ c (v1 : v2 : Nil)) | c == cCons =
      case expand us (V.Constr false cCons (V.Hole false : V.Hole false : Nil)) of
         V.Constr _ _ (u1 : u2 : Nil) -> (u1 × v1) A.: from (u2 × v2)
