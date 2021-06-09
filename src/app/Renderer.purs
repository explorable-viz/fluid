module App.Renderer where

import Prelude
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.Array (zip, zipWith)
import Data.List (List(..), (:))
import Data.Tuple (fst)
import Data.Profunctor.Strong (first)
import Bindings (Bindings, Bind, Var, (↦), find)
import DataType (cBarChart, cCons, cNil)
import Effect (Effect)
import Lattice (𝔹, expand)
import Primitive (Slice, class ToFrom, as, match, match_bwd, match_fwd)
import Util (type (×), (×), type (+), successful)
import Util.SnocList (SnocList(..), (:-))
import Val (Array2, MatrixRep, Val)
import Val (Val(..)) as V

foreign import drawFigure :: String -> Array Fig -> Effect Unit

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to specific examples for now. Matrices are assumed to have element type Int.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
type BarChart = { caption :: String × 𝔹, data_ :: Array BarChartRecord }
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

-- Convert list slice to array of slices, with hole expansion as necessary, discarding list-level annotations.
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
makeBarChart title _ (u × V.Constr _ c (v1 : Nil)) | c == cBarChart =
   case expand u (V.Constr false cBarChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) ->
         let { caption: caption × _, data_ } = record barChart (u1 × v1) in BarChart { caption, data: data_ }

lineChart :: MakeFig
lineChart title _ _ = LineChart { title }

record :: forall a . (Slice (Bindings (Val 𝔹)) -> a) -> Slice (Val 𝔹) -> a
record toRecord (u × v) = toRecord (fst (match_fwd (u × v)) × fst (match v))

energyRecord :: Slice (Bindings (Val 𝔹)) -> EnergyRecord
energyRecord r = {
   year: get_prim "year" r,
   country: get_prim "country" r,
   energyType: get_prim "energyType" r,
   output: get_intNumber "output" r
}

barChart :: Partial => Slice (Bindings (Val 𝔹)) -> BarChart
barChart r = {
   caption: get_prim "caption" r,
   data_: record from <$> toArray (get "data" r)
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

class Reflect a b | a -> b where
   from :: Slice a -> b
   to :: b -> a

instance reflectBarChartRecord :: Reflect (SnocList (Bind (Val Boolean)))
                                  { x :: String × Boolean, y :: Number × Boolean } where
   from r = {
      x: get_prim "x" r,
      y: get_intNumber "y" r
   }

   to { x, y } = Lin :- "x" ↦ match_bwd x :- "y" ↦ match_bwd y
