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

foreign import drawFigure :: String -> MatrixFig -> MatrixFig -> MatrixFig -> Effect Unit

-- Will want to generalise to arrays of "drawable values". Second component of elements is original value.
toIntMatrix :: Array2 (Val 𝔹 × Val 𝔹) -> Array2 (Int × 𝔹)
toIntMatrix = (<$>) ((<$>) match_fwd)

bits :: MatrixRep 𝔹 × MatrixRep 𝔹 -> MatrixRep'
bits ((vss × _ × _) × (vss' × (i × _) × (j × _))) = toIntMatrix (zipWith zip vss vss') × i × j

-- Inputs are pairs of matrices; second component is original (unsliced) matrix.
renderFigure :: String -> Val 𝔹 × Val 𝔹 -> Val 𝔹 × Val 𝔹 -> Val 𝔹 × Val 𝔹 -> Effect Unit
renderFigure id (o × o') (ω × ω') (i × i') =
   let i'' × _ = match_fwd (i × i')
       ω'' × _ = match_fwd (ω × ω')
       o'' × _ = match_fwd (o × o')
   in drawFigure id { title: "output", cellFillSelected: "Yellow", matrix: bits (o'' × fst (match o')) }
                    { title: "filter", cellFillSelected: "LightGreen", matrix: bits (ω'' × fst (match ω')) }
                    { title: "input", cellFillSelected: "LightGreen", matrix: bits (i'' × fst (match i')) }
