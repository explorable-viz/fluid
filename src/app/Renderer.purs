module App.Renderer where

import Prelude
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

-- Similar to MatrixRep 𝔹, but with elements converted from values to the underlying data type.
type MatrixRep' = Array2 (Int × 𝔹) × Int × Int

-- Hardcode to specific example for now.
type RecordRep = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Int × 𝔹 }
type TableRep = Array RecordRep

data Fig =
   MatrixFig { title :: String, cellFillSelected :: String, matrix :: MatrixRep' } |
   TableFig { title :: String, cellFillSelected :: String, table :: TableRep }

type FigConstructor = String -> String -> Val 𝔹 × Val 𝔹 -> Fig

matrixFig :: FigConstructor
matrixFig title cellFillSelected (u × v) =
   let v' × _ = match_fwd (u × v) in
   MatrixFig { title, cellFillSelected, matrix: matrixRep (v' × fst (match v)) }

-- Discard annotations on the list itself.
tableFig :: FigConstructor
tableFig title cellFillSelected (u × v) =
   TableFig { title, cellFillSelected, table: fromFoldable (recordRep' <$> (L.zip (toList u) (toList v))) }

recordRep' :: Val 𝔹 × Val 𝔹 -> RecordRep
recordRep' (u × v) =
   let v' × _ = match_fwd (u × v) in
   recordRep (v' × fst (match v))

foreign import drawBarChart :: String -> Effect Unit
foreign import drawFigure :: String -> Array Fig -> Effect Unit

-- Will want to generalise to arrays of "drawable values". Second component of elements is original value.
matrixRep :: MatrixRep 𝔹 × MatrixRep 𝔹 -> MatrixRep'
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) = toMatrix (zipWith zip vss uss) × i × j
   where toMatrix :: forall a . ToFrom a => Array2 (Val 𝔹 × Val 𝔹) -> Array2 (a × 𝔹)
         toMatrix = (<$>) ((<$>) match_fwd)

recordRep :: Bindings (Val 𝔹) × Bindings (Val 𝔹) -> RecordRep
recordRep xvs2 =
   { year: get "year" xvs2, country: get "country" xvs2, energyType: get "energyType" xvs2, output: get "output" xvs2 }
   where get :: forall a . ToFrom a => Var -> Bindings (Val 𝔹) × Bindings (Val 𝔹) -> a × 𝔹
         get x (xvs × xus) = match_fwd (successful (find x xvs) × successful (find x xus))
