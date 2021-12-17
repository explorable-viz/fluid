module App.Renderer where

import Prelude
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Tuple (fst)
import Effect.Console (log)
import Web.Event.EventTarget (eventListener)
import Web.Event.Internal.Types (Event)
import App.BarChart (BarChart, drawBarChart)
import App.LineChart (LineChart, drawLineChart)
import App.MatrixView (MatrixView(..), drawMatrix, matrixRep)
import App.TableView (EnergyTable(..), drawTable, energyRecord)
import App.Util (HTMLId, from, record)
import DataType (cBarChart, cCons, cLineChart, cNil)
import Effect (Effect)
import Lattice (𝔹, expand)
import Primitive (Slice, match, match_fwd)
import Util ((×))
import Val (Val)
import Val (Val(..)) as V

type Fig = {
   divId :: HTMLId,
   subfigs :: Array SubFig
}

drawFig :: Fig -> Effect Unit
drawFig { divId, subfigs } =
   sequence_ $ drawSubFig divId <$> subfigs

data SubFig =
   MatrixFig MatrixView |
   EnergyTableView EnergyTable |
   LineChartFig LineChart |
   BarChartFig BarChart

myHandler :: Event -> Effect Unit
myHandler _ = do
   log "Hello"
   pure unit

drawSubFig :: HTMLId -> SubFig -> Effect Unit
drawSubFig divId (MatrixFig fig) = drawMatrix divId fig
drawSubFig divId (EnergyTableView fig) = drawTable divId fig
drawSubFig divId (LineChartFig fig) = drawLineChart divId fig
drawSubFig divId (BarChartFig fig) = drawBarChart divId fig =<< eventListener myHandler

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
