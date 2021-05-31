module App.Renderer where

import Prelude
import Data.Array (zip, zipWith)
import Data.Tuple (fst)
import Effect (Effect)
import Lattice (𝔹)
import Primitive (match, match_fwd)
import Util (type (×), (×))
import Val (Array2, MatrixRep, Val)

-- Similar to MatrixRep 𝔹, but with elements converted from values to the underlying data type.
type MatrixRep' = Array2 (Int × 𝔹) × Int × Int
type MatrixFig = { title :: String, cellFillSelected :: String, matrix :: MatrixRep' }

matrixFig :: String -> String -> Val 𝔹 × Val 𝔹 -> MatrixFig
matrixFig title cellFillSelected (u × v) =
   let v' × _ = match_fwd (u × v) in
   { title, cellFillSelected, matrix: bits (v' × fst (match v)) }

foreign import drawBarChart :: String -> Effect Unit
foreign import drawFigure :: String -> Array MatrixFig -> Effect Unit
foreign import drawTable :: String -> Effect Unit

-- Will want to generalise to arrays of "drawable values". Second component of elements is original value.
toIntMatrix :: Array2 (Val 𝔹 × Val 𝔹) -> Array2 (Int × 𝔹)
toIntMatrix = (<$>) ((<$>) match_fwd)

bits :: MatrixRep 𝔹 × MatrixRep 𝔹 -> MatrixRep'
bits ((vss × _ × _) × (vss' × (i × _) × (j × _))) = toIntMatrix (zipWith zip vss vss') × i × j
