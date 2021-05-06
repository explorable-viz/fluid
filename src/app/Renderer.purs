module App.Renderer where

import Prelude
import Data.Array (zip, zipWith)
import Data.Tuple (fst)
import Effect (Effect)
import Lattice (𝔹)
import Primitive (match, match_fwd)
import Util (type (×), (×))
import Val (Array2, MatrixRep, Val)

-- Similar to MatrixRep 𝔹, but with elements converted from values to the underlying data.
type MatrixRep' = Array2 (Int × 𝔹) × Int × Int
type MatrixFig = { title :: String, matrix :: MatrixRep' }

foreign import drawFigure :: String -> MatrixFig -> MatrixFig -> MatrixFig -> Effect Unit

-- Will want to generalise to arrays of "drawable values". Second component of elements is original value.
toIntMatrix :: Array2 (Val 𝔹 × Val 𝔹) -> Array2 (Int × 𝔹)
toIntMatrix = (<$>) ((<$>) match_fwd)

bits :: MatrixRep 𝔹 × MatrixRep 𝔹 -> MatrixRep'
bits ((vss × _ × _) × (vss' × (i × _) × (j × _))) = toIntMatrix (zipWith zip vss vss') × i × j

-- Inputs are pairs of matrices; second component is original (unsliced) matrix.
renderFigure :: String -> Val 𝔹 × Val 𝔹 -> Val 𝔹 × Val 𝔹 -> Val 𝔹 × Val 𝔹 -> Effect Unit
renderFigure id (output × output') (filter × filter') (input × input') =
   let input'' × _ = match_fwd (input × input')
       filter'' × _ = match_fwd (filter × filter')
       output'' × _ = match_fwd (output × output')
   in drawFigure id { title: "output", matrix: bits (output'' × fst (match output')) }
                    { title: "filter", matrix: bits (filter'' × fst (match filter')) }
                    { title: "input", matrix: bits (input'' × fst (match input')) }
