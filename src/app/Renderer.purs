module App.Renderer where

import Prelude
import Control.Apply (lift2)
import Data.Array (fromFoldable, zip, zipWith)
import Data.List (zip) as L
import Data.Tuple (fst)
import Effect (Effect)
import Bindings (Bindings, Var, find)
import Lattice (𝔹)
import Pretty (toList)
import Primitive (class ToFrom, match, match_fwd)
import Util (type (×), (×), successful)
import Val (Array2, MatrixRep, Val)

foreign import drawBarChart :: String -> Effect Unit
foreign import drawFigure :: String -> Array Fig -> Effect Unit

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to specific examples for now. Matrices are assumed to have element type Int.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Int × 𝔹 }

data Fig =
   MatrixFig { title :: String, cellFillSelected :: String, matrix :: IntMatrix } |
   EnergyTable { title :: String, cellFillSelected :: String, table :: Array EnergyRecord } |
   LineChart { title :: String }

-- Convert sliced value to appropriate Fig, discarding top-level annotations for now. As elsewhere, second
-- component of pair is original (unsliced) value, to allow for hole-expansion.
type MakeFig = String -> String -> Val 𝔹 × Val 𝔹 -> Fig

matrixFig :: MakeFig
matrixFig title cellFillSelected (u × v) =
   let vss × _ = match_fwd (u × v) in
   MatrixFig { title, cellFillSelected, matrix: matrixRep (vss × fst (match v)) }

energyTable :: MakeFig
energyTable title cellFillSelected (u × v) =
   EnergyTable { title, cellFillSelected, table: fromFoldable (energyRecord <$> (L.zip (toList u) (toList v))) }

lineChart :: MakeFig
lineChart title _ _ = LineChart { title }

energyRecord :: Val 𝔹 × Val 𝔹 -> EnergyRecord
energyRecord (u × v) =
   let xvs × _ = match_fwd (u × v) in
   toEnergyRecord (xvs × fst (match v))
   where
   toEnergyRecord :: Bindings (Val 𝔹) × Bindings (Val 𝔹) -> EnergyRecord
   toEnergyRecord xvs2 =
      { year: get "year" xvs2, country: get "country" xvs2, energyType: get "energyType" xvs2, output: get "output" xvs2 }

matrixRep :: MatrixRep 𝔹 × MatrixRep 𝔹 -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) = toMatrix (zipWith zip vss uss) × i × j
   where toMatrix :: forall a . ToFrom a => Array2 (Val 𝔹 × Val 𝔹) -> Array2 (a × 𝔹)
         toMatrix = (<$>) ((<$>) match_fwd)

get :: forall a . ToFrom a => Var -> Bindings (Val 𝔹) × Bindings (Val 𝔹) -> a × 𝔹
get x (xvs × xus) = successful $
   match_fwd <$> (find x xvs `lift2 (×)` find x xus)
