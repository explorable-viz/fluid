module App.Renderer where

import Prelude
import Data.Array (zip, zipWith)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (fst)
import App.BarChart (BarChart, drawBarChart)
import App.LineChart (LineChart, drawLineChart)
import App.MatrixView (MatrixView(..), IntMatrix, drawMatrix)
import App.Util (HTMLId, get_intOrNumber, get_prim, from, record)
import Bindings (Bindings)
import DataType (cBarChart, cCons, cLineChart, cNil)
import Effect (Effect)
import Lattice (𝔹, expand)
import Primitive (Slice, match, match_fwd)
import Util (type (×), (×))
import Val (MatrixRep, Val)
import Val (Val(..)) as V

type Fig = {
   divId :: HTMLId,
   subfigs :: Array SubFig
}

drawFig :: Fig -> Effect Unit
drawFig { divId, subfigs } =
   sequence_ $ drawSubFig divId <$> subfigs

foreign import drawTable :: HTMLId -> EnergyTable -> Effect Unit

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to specific examples for now. Matrices are assumed to have element type Int.
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
newtype EnergyTable = EnergyTable { title :: String, table :: Array EnergyRecord }

data SubFig =
   MatrixFig MatrixView |
   EnergyTableView EnergyTable |
   LineChartFig LineChart |
   BarChartFig BarChart

drawSubFig :: HTMLId -> SubFig -> Effect Unit
drawSubFig divId (MatrixFig fig) = drawMatrix divId fig
drawSubFig divId (EnergyTableView fig) = drawTable divId fig
drawSubFig divId (LineChartFig fig) = drawLineChart divId fig
drawSubFig divId (BarChartFig fig) = drawBarChart divId fig

-- Convert sliced value to appropriate SubFig, discarding top-level annotations for now.
makeSubFig :: Partial => { title :: String, uv :: Slice (Val 𝔹) } -> SubFig
makeSubFig { title, uv: u × V.Constr _ c (v1 : Nil) } | c == cBarChart =
   case expand u (V.Constr false cBarChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) -> BarChartFig (record from (u1 × v1))
makeSubFig { title, uv: u × V.Constr _ c (v1 : Nil) } | c == cLineChart =
   case expand u (V.Constr false cLineChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) -> LineChartFig (record from (u1 × v1))
makeSubFig { title, uv: u × v@(V.Constr _ c _) } | c == cNil || c == cCons =
   EnergyTableView (EnergyTable { title, table: record energyRecord <$> from (u × v) })
makeSubFig { title, uv: u × v@(V.Matrix _ _) } =
   let vss2 = fst (match_fwd (u × v)) × fst (match v) in
   MatrixFig (MatrixView { title, matrix: matrixRep vss2 } )

energyRecord :: Slice (Bindings (Val 𝔹)) -> EnergyRecord
energyRecord r = {
   year: get_prim "year" r,
   country: get_prim "country" r,
   energyType: get_prim "energyType" r,
   output: get_intOrNumber "output" r
}

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) (zipWith zip vss uss) × i × j
