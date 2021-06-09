module App.Renderer where

import Prelude
import Bindings (Bindings, Var, find)
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.Array (zip, zipWith)
import Data.List (List(..), (:))
import Data.Tuple (fst)
import DataType (cBarChart, cCons, cNil)
import Effect (Effect)
import Lattice (𝔹, expand)
import Primitive (Slice, class ToFrom, as, match, match_fwd)
import Util (type (×), (×), type (+), successful)
import Val (Array2, MatrixRep, Val)
import Val (Val(..)) as V

foreign import drawFigure :: String -> Array Fig -> Effect Unit

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to specific examples for now. Matrices are assumed to have element type Int.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
type BarChart = { caption :: String × 𝔹, data_ :: Array BarChartRecord × 𝔹 }
type BarChartRecord = { x :: String × 𝔹, y :: Number × 𝔹 }

data Fig =
   MatrixFig { title :: String, cellFillSelected :: String, matrix :: IntMatrix } |
   EnergyTable { title :: String, cellFillSelected :: String, table :: Array EnergyRecord } |
   LineChart { title :: String } |
   BarChart { caption :: String, data :: Array BarChartRecord }

-- Convert sliced value to appropriate Fig, discarding top-level annotations for now.
type MakeFig = Partial => String -> String -> Slice (Val 𝔹) -> Fig

matrixFig :: MakeFig
matrixFig title cellFillSelected (u × v) =
   let vss2 = fst (match_fwd (u × v)) × fst (match v) in
   MatrixFig { title, cellFillSelected, matrix: matrixRep vss2 }

-- Convert a list slice to an array of slices, with hole expansion as necessary, discarding list-level annotations.
toArray :: Partial => Slice (Val 𝔹) -> Array (Slice (Val 𝔹))
toArray (vs × V.Constr _ c Nil) | c == cNil =
   case expand vs (V.Constr false cNil Nil) of
      V.Constr _ _ Nil -> []
toArray (us × V.Constr _ c (v1 : v2 : Nil)) | c == cCons =
   case expand us (V.Constr false cCons (V.Hole false : V.Hole false : Nil)) of
      V.Constr _ _ (u1 : u2 : Nil) -> (u1 × v1) A.: toArray (u2 × v2)

makeEnergyTable :: MakeFig
makeEnergyTable title cellFillSelected (u × v) =
   EnergyTable { title, cellFillSelected, table: record energyRecord <$> toArray (u × v) }

makeBarChart :: MakeFig
makeBarChart title _ (u × V.Constr _ c (V.Record _ xvs : Nil)) | c == cBarChart =
   case expand u (V.Constr false cBarChart (V.Record false (const (V.Hole false) <$> xvs) : Nil)) of
      V.Constr _ _ (V.Record _ xus : Nil) ->
         let { caption, data_ } = record barChart (xus × xvs) in BarChart { caption, data: data_ }

lineChart :: MakeFig
lineChart title _ _ = LineChart { title }

record :: forall a . (Slice (Bindings (Val 𝔹)) -> a) -> Slice (Val 𝔹) -> a
record toRecord (u × v) =
   toRecord (fst (match_fwd (u × v)) × fst (match v))

energyRecord :: Slice (Bindings (Val 𝔹)) -> EnergyRecord
energyRecord xvs2 = {
   year: get "year" xvs2,
   country: get "country" xvs2,
   energyType: get "energyType" xvs2,
   -- TODO: extract helper for this
   output: let n × α = get "output" xvs2 :: (Int + Number) × 𝔹 in as n × α
}

barChart :: Slice (Bindings (Val 𝔹)) -> BarChart
barChart xvs2 = { caption: get "caption" xvs2, data_: ?_ {-get "data" xvs2-} }

barChartRecord :: Slice (Bindings (Val 𝔹)) -> BarChartRecord
barChartRecord xvs2 = { x: get "x" xvs2, y: get_intNumber"y" xvs2 }

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) = toMatrix (zipWith zip vss uss) × i × j
   where toMatrix :: forall a . ToFrom a => Array2 (Val 𝔹 × Val 𝔹) -> Array2 (a × 𝔹)
         toMatrix = (<$>) ((<$>) match_fwd)

get :: forall a . ToFrom a => Var -> Slice (Bindings (Val 𝔹)) -> a × 𝔹
get x (xvs × xus) = successful $
   match_fwd <$> (find x xvs `lift2 (×)` find x xus)

get_intNumber :: Var -> Slice (Bindings (Val 𝔹)) -> Number × 𝔹
get_intNumber x xvs2 = let n × α = get x xvs2 :: (Int + Number) × 𝔹 in as n × α
