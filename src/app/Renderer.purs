module App.Renderer where

import Prelude
import Data.Array (zip, zipWith)
import Data.Tuple (fst)
import Effect (Effect)
import Lattice (𝔹)
import Primitive (match, match_fwd)
import Util (type (×), (×))
import Val (Array2, MatrixRep, Val)

foreign import drawMatrix :: Array2 (Int × 𝔹) -> Int -> Int -> Effect Unit

-- Will want to generalise to arrays of "drawable values". Second component of elements is original value.
toIntMatrix :: Array2 (Val 𝔹 × Val 𝔹) -> Array2 (Int × 𝔹)
toIntMatrix = (<$>) ((<$>) match_fwd)

-- Inputs are matrices; second is original (unsliced) value.
renderMatrix :: Val 𝔹 × Val 𝔹 -> Effect Unit
renderMatrix (v × v') =
   let u × α = match_fwd (v × v') in renderMatrix' (u × fst (match v') × α)
   where
   renderMatrix' :: MatrixRep 𝔹 × MatrixRep 𝔹 × 𝔹 -> Effect Unit
   renderMatrix' ((vss × _ × _) × (vss' × (i × _) × (j × _)) × _) = drawMatrix (toIntMatrix (zipWith zip vss vss')) i j
