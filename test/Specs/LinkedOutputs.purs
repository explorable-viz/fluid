module Test.Specs.LinkedOutputs where

import Prelude

import App.Util.Selector (barChart, barSegment, dictVal, fst, lineChart, linePoint, listElement, matrixElement, multiViewEntry, scatterPlot, scatterPoint, snd)
import Bind ((↦))
import Data.Maybe (Maybe(..))
import DataType (f_plots, f_y)
import Graph (DVertex'(..))
import Lattice (neg)
import Module.Web (File(..), Folder(..))
import Test.Util.Suite (TestLinkedOutputsSpec)
import Util ((×))
import Val (BaseVal(..), MatrixDim(..), MatrixRep(..), Val(..), asVal)

linkedOutputs_spec1 :: TestLinkedOutputsSpec
linkedOutputs_spec1 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , datasets: [ "renewables" ↦ "dataset/renewables" ]
        , imports: []
        , file: File "slicing/linked-outputs/bar-chart-line-chart"
        , inputs: [ "renewables" ]
        , query: Nothing
        }
   , δ_out: multiViewEntry "barChart" (barChart (barSegment 1 0 neg))
   , out_expect:
        multiViewEntry "barChart" (barChart (barSegment 1 0 neg))
           >>> multiViewEntry "lineChart"
              ( lineChart
                   ( dictVal f_plots
                        ( listElement 0 (linePoint 2 (dictVal f_y neg))
                             >>> listElement 1 (linePoint 2 (dictVal f_y neg))
                             >>> listElement 2 (linePoint 2 (dictVal f_y neg))
                             >>> listElement 3 (linePoint 2 (dictVal f_y neg))

                        )
                   )
              )
   }

linkedOutputs_spec2 :: TestLinkedOutputsSpec
linkedOutputs_spec2 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , datasets:
             [ "renewables" ↦ "dataset/renewables-new"
             , "nonRenewables" ↦ "dataset/non-renewables"
             ]
        , imports: []
        , file: File "slicing/linked-outputs/stacked-bar-scatter-plot"
        , inputs: [ "nonRenewables" ]
        , query: Nothing
        }
   , δ_out: multiViewEntry "stackedBarChart" (barChart (barSegment 3 2 neg >>> barSegment 4 1 neg >>> barSegment 4 3 neg))
   , out_expect:
        multiViewEntry "stackedBarChart" (barChart (barSegment 3 2 neg >>> barSegment 4 1 neg >>> barSegment 4 3 neg))
           >>> multiViewEntry "scatterPlot"
              ( scatterPlot
                   ( scatterPoint 4 (dictVal f_y neg)
                        >>> scatterPoint 6 (dictVal f_y neg)
                   )
              )
   }

movingAverages_spec :: TestLinkedOutputsSpec
movingAverages_spec =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , datasets: [ "methane" ↦ "dataset/methane-emissions" ]
        , imports: []
        , file: File "linked-outputs/moving-average"
        , inputs: [ "methane" ]
        , query: Nothing
        }
   , δ_out: identity -- TODO: make this a non-trivial test
   , out_expect: identity
   }

linkedOutputs_cases :: Array TestLinkedOutputsSpec
linkedOutputs_cases =
   [ { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
          , datasets: [ "data" ↦ "linked-outputs/pairs-data" ]
          , imports: []
          , file: File "linked-outputs/pairs"
          , inputs: [ "data" ]
          , query: Nothing
          }
     , δ_out: snd neg
     , out_expect: neg
     }
   , { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
          , datasets: [ "data" ↦ "linked-outputs/convolution-data" ]
          , imports: [ "lib/convolution" ]
          , file: File "linked-outputs/convolution"
          , inputs: [ "data" ]
          , query:
               Just $ asVal >=> case _ of
                  v@(Val α (Matrix (MatrixRep (_ × MatrixDim (3 × _) × MatrixDim (3 × _))))) -> Just $ DVertex (α × v)
                  _ -> Nothing

          }
     , δ_out: fst (matrixElement 2 2 neg)
     , out_expect:
          fst
             ( matrixElement 2 1 neg
                  >>> matrixElement 2 2 neg
                  >>> matrixElement 2 3 neg
                  >>> matrixElement 2 4 neg
                  >>> matrixElement 2 5 neg
             )
             >>> snd
                ( matrixElement 1 1 neg
                     >>> matrixElement 1 2 neg
                     >>> matrixElement 1 3 neg
                     >>> matrixElement 2 1 neg
                     >>> matrixElement 2 2 neg
                     >>> matrixElement 2 3 neg
                     >>> matrixElement 3 1 neg
                     >>> matrixElement 3 2 neg
                     >>> matrixElement 3 3 neg
                )
     }
   , linkedOutputs_spec1
   , linkedOutputs_spec2
   , movingAverages_spec
   ]
