module Test.Specs.LinkedOutputs where

import Prelude

import App.Util (SelectionType(..))
import App.Util.Selector (barChart, barSegment, dictVal, fst, lineChart, linePoint, listElement, matrixElement, multiViewEntry, scatterPlot, scatterPoint, snd, (>.>), select)
import Bind ((↦))
import Data.Maybe (Maybe(..))
import DataType (f_plots, f_y)
import File (File(..), Folder(..))
import Test.Util.Suite (TestLinkedOutputsSpec)
import Util ((×))

linkedOutputs_spec1 :: TestLinkedOutputsSpec
linkedOutputs_spec1 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , datasets: [ "renewables" ↦ "dataset/renewables" ]
        , imports: []
        , file: File "slicing/linked-outputs/bar-chart-line-chart"
        , inputs: [ "renewables" ]
        , query: Nothing
        , linking: true
        }
   , δ_out: multiViewEntry "barChart" (barChart (barSegment 1 0 select))
   , out_expect:
        multiViewEntry "barChart" (barChart (barSegment 1 0 select))
           >.> multiViewEntry "lineChart"
              ( lineChart
                   ( dictVal f_plots
                        ( listElement 0 (linePoint 2 (dictVal f_y select))
                             >.> listElement 1 (linePoint 2 (dictVal f_y select))
                             >.> listElement 2 (linePoint 2 (dictVal f_y select))
                             >.> listElement 3 (linePoint 2 (dictVal f_y select))

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
        , linking: true
        }
   , δ_out: multiViewEntry "stackedBarChart" (barChart (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
   , out_expect:
        multiViewEntry "stackedBarChart" (barChart (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
           >.> multiViewEntry "scatterPlot"
              ( scatterPlot
                   ( scatterPoint 4 (dictVal f_y select)
                        >.> scatterPoint 6 (dictVal f_y select)
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
        , linking: true
        }
   , δ_out: identity >>> (_ × Persistent) -- TODO: make this a non-trivial test
   , out_expect: identity >>> (_ × Persistent)
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
          , linking: true
          }
     , δ_out: snd select
     , out_expect: select
     }
   , { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
          , datasets: [ "data" ↦ "linked-outputs/convolution-data" ]
          , imports: [ "lib/matrix" ]
          , file: File "linked-outputs/convolution"
          , inputs: [ "data" ]
          , query: Nothing
          , linking: true
          }
     , δ_out: fst (matrixElement 2 2 select)
     , out_expect:
          fst
             ( matrixElement 2 1 select
                  >.> matrixElement 2 2 select
                  >.> matrixElement 2 3 select
                  >.> matrixElement 2 4 select
                  >.> matrixElement 2 5 select
             )
             >.> snd
                ( matrixElement 1 1 select
                     >.> matrixElement 1 2 select
                     >.> matrixElement 1 3 select
                     >.> matrixElement 2 1 select
                     >.> matrixElement 2 2 select
                     >.> matrixElement 2 3 select
                     >.> matrixElement 3 1 select
                     >.> matrixElement 3 2 select
                     >.> matrixElement 3 3 select
                )
     }
   , linkedOutputs_spec1
   , linkedOutputs_spec2
   , movingAverages_spec
   ]
