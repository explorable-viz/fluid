module App.Renderer where

import Prelude
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.Array (zip, zipWith)
import Data.List (List(..), (:))
import Data.Tuple (fst)
import Data.Profunctor.Strong (first)
import Bindings (Bindings, Bind, Var, find)
import DataType (cBarChart, cCons, cNil)
import Effect (Effect)
import Lattice (𝔹, expand)
import Primitive (Slice, class ToFrom, as, match, match_fwd)
import Util (type (×), (×), type (+), successful)
import Util.SnocList (SnocList)
import Val (Array2, MatrixRep, Val)
import Val (Val(..)) as V

foreign import drawFigure :: String -> Array Fig -> Effect Unit

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to specific examples for now. Matrices are assumed to have element type Int.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
newtype BarChart = BarChart { caption :: String × 𝔹, data_ :: Array BarChartRecord }
newtype BarChartRecord = BarChartRecord { x :: String × 𝔹, y :: Number × 𝔹 }

data Fig =
   MatrixFig { title :: String, cellFillSelected :: String, matrix :: IntMatrix } |
   EnergyTable { title :: String, cellFillSelected :: String, table :: Array EnergyRecord } |
   LineChart { title :: String } |
   BarChartFig BarChart

-- Convert sliced value to appropriate Fig, discarding top-level annotations for now.
type MakeFig = { title :: String, uv :: Slice (Val 𝔹) } -> Fig

matrixFig :: MakeFig
matrixFig { title, uv: (u × v) } =
   let vss2 = fst (match_fwd (u × v)) × fst (match v) in
   MatrixFig { title, cellFillSelected: "Yellow", matrix: matrixRep vss2 }

toArray :: Partial => Slice (Val 𝔹) -> Array (Slice (Val 𝔹))
toArray (vs × V.Constr _ c Nil) | c == cNil =
   case expand vs (V.Constr false cNil Nil) of
      V.Constr _ _ Nil -> []
toArray (us × V.Constr _ c (v1 : v2 : Nil)) | c == cCons =
   case expand us (V.Constr false cCons (V.Hole false : V.Hole false : Nil)) of
      V.Constr _ _ (u1 : u2 : Nil) -> (u1 × v1) A.: toArray (u2 × v2)

makeEnergyTable :: Partial => MakeFig
makeEnergyTable { title, uv: (u × v) } =
   EnergyTable { title, cellFillSelected: "Not used?", table: record energyRecord <$> toArray (u × v) }

makeBarChart :: Partial => MakeFig
makeBarChart { title, uv: u × V.Constr _ c (v1 : Nil) } | c == cBarChart =
   case expand u (V.Constr false cBarChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) -> BarChartFig (record from (u1 × v1))

lineChart :: MakeFig
lineChart { title } = LineChart { title }

record :: forall a . (Slice (Bindings (Val 𝔹)) -> a) -> Slice (Val 𝔹) -> a
record toRecord (u × v) = toRecord (fst (match_fwd (u × v)) × fst (match v))

energyRecord :: Slice (Bindings (Val 𝔹)) -> EnergyRecord
energyRecord r = {
   year: get_prim "year" r,
   country: get_prim "country" r,
   energyType: get_prim "energyType" r,
   output: get_intNumber "output" r
}

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) (zipWith zip vss uss) × i × j

get_prim :: forall a . ToFrom a => Var -> Slice (Bindings (Val 𝔹)) -> a × 𝔹
get_prim x = match_fwd <<< get x

get_intNumber :: Var -> Slice (Bindings (Val 𝔹)) -> Number × 𝔹
get_intNumber x r = first as (get_prim x r :: (Int + Number) × 𝔹)

get :: Var -> Slice (Bindings (Val 𝔹)) -> Slice (Val 𝔹)
get x (r × r') = successful $ find x r `lift2 (×)` find x r'

class Reflect a b where
   from :: Partial => Slice a -> b

instance reflectBarChartRecord :: Reflect (SnocList (Bind (Val Boolean))) BarChartRecord where
   from r = BarChartRecord {
      x: get_prim "x" r,
      y: get_intNumber "y" r
   }

instance reflectBarChart :: Reflect (SnocList (Bind (Val Boolean))) BarChart where
   from r = BarChart {
      caption: get_prim "caption" r,
      data_: record from <$> from (get "data" r)
   }

-- Hole expansion as necessary; discards list-level annotations.
instance reflectArray :: Reflect (Val Boolean) (Array (Val Boolean × Val Boolean)) where
   from (vs × V.Constr _ c Nil) | c == cNil =
      case expand vs (V.Constr false cNil Nil) of
         V.Constr _ _ Nil -> []
   from (us × V.Constr _ c (v1 : v2 : Nil)) | c == cCons =
      case expand us (V.Constr false cCons (V.Hole false : V.Hole false : Nil)) of
         V.Constr _ _ (u1 : u2 : Nil) -> (u1 × v1) A.: from (u2 × v2)
