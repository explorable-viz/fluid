module App.Renderer where

import Prelude hiding (absurd)

import Bindings (Bindings, Var, find)
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.Array (zip, zipWith)
import Data.List (List(..), (:))
import Data.Tuple (fst)
import DataType (cCons, cNil)
import Effect (Effect)
import Lattice (𝔹, expand)
import Primitive (Slice, class ToFrom, as, match, match_fwd)
import Util (type (×), (×), type (+), absurd, error, successful)
import Val (Array2, MatrixRep, Val)
import Val (Val(..)) as V

foreign import drawFigure :: String -> Array Fig -> Effect Unit

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to specific examples for now. Matrices are assumed to have element type Int.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
type BarChartRecord = { x :: String × 𝔹, y :: Number × 𝔹 }

data Fig =
   MatrixFig { title :: String, cellFillSelected :: String, matrix :: IntMatrix } |
   EnergyTable { title :: String, cellFillSelected :: String, table :: Array EnergyRecord } |
   LineChart { title :: String } |
   BarChart { title :: String, data :: Array BarChartRecord }

-- Convert sliced value to appropriate Fig, discarding top-level annotations for now.
type MakeFig = String -> String -> Slice (Val 𝔹) -> Fig

matrixFig :: MakeFig
matrixFig title cellFillSelected (u × v) =
   let vss2 = fst (match_fwd (u × v)) × fst (match v) in
   MatrixFig { title, cellFillSelected, matrix: matrixRep vss2 }

-- Convert a list slice to an array of slices, with hole expansion as necessary, discarding list-level annotations.
toArray :: Slice (Val 𝔹) -> Array (Slice (Val 𝔹))
toArray (vs × V.Constr _ c Nil) | c == cNil =
   case expand vs (V.Constr false cNil Nil) of
      V.Constr _ _ Nil -> []
      _ -> error absurd
toArray (us × V.Constr _ c (v1 : v2 : Nil)) | c == cCons =
   case expand us (V.Constr false cCons (V.Hole false : V.Hole false : Nil)) of
      V.Constr _ _ (u1 : u2 : Nil) -> (u1 × v1) A.: toArray (u2 × v2)
      _ -> error absurd
toArray _ = error absurd

energyTable :: MakeFig
energyTable title cellFillSelected (u × v) =
   EnergyTable { title, cellFillSelected, table: record energyRecord <$> toArray (u × v) }

barChart :: MakeFig
barChart title _ (u × v) = BarChart { title, data: record barChartRecord <$> toArray (u × v) }

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
   output: let n × α = get "output" xvs2 :: (Int + Number) × 𝔹 in as n × α
}

barChartRecord :: Slice (Bindings (Val 𝔹)) -> BarChartRecord
barChartRecord xvs2 = {
   x: get "x" xvs2,
   y: let n × α = get "y" xvs2 :: (Int + Number) × 𝔹 in as n × α
}

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) = toMatrix (zipWith zip vss uss) × i × j
   where toMatrix :: forall a . ToFrom a => Array2 (Val 𝔹 × Val 𝔹) -> Array2 (a × 𝔹)
         toMatrix = (<$>) ((<$>) match_fwd)

get :: forall a . ToFrom a => Var -> Slice (Bindings (Val 𝔹)) -> a × 𝔹
get x (xvs × xus) = successful $
   match_fwd <$> (find x xvs `lift2 (×)` find x xus)
