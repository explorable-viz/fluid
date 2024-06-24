module Test.Specs.LinkedOutputs where

import Prelude

import App.Util.Selector (barChart, barSegment, field, fst, lineChart, linePoint, listElement, matrixElement, multiPlotEntry, scatterPlot, scatterPoint, snd)
import Bind ((↦))
import DataType (f_plots, f_y)
import Lattice (neg)
import Module (File(..))
import Test.Util.Suite (TestLinkedOutputsSpec)

linkedOutputs_spec1 :: TestLinkedOutputsSpec
linkedOutputs_spec1 =
   { spec:
        { datasets: [ "renewables" ↦ "example/linked-outputs/renewables" ]
        , imports: []
        , file: File "linked-outputs/bar-chart-line-chart"
        , inputs: [ "renewables" ]
        }
   , δ_out: multiPlotEntry "bar-chart" (barChart (barSegment 1 0 neg))
   , out_expect:
        multiPlotEntry "bar-chart" (barChart (barSegment 1 0 neg))
           >>> multiPlotEntry "line-chart"
              ( lineChart
                   ( field f_plots
                        ( listElement 0 (linePoint 2 (field f_y neg))
                             >>> listElement 1 (linePoint 2 (field f_y neg))
                             >>> listElement 2 (linePoint 2 (field f_y neg))
                             >>> listElement 3 (linePoint 2 (field f_y neg))
                        )
                   )
              )
   }

linkedOutputs_spec2 :: TestLinkedOutputsSpec
linkedOutputs_spec2 =
   { spec:
        { datasets:
             [ "renewables" ↦ "example/linked-inputs/renewables"
             , "nonRenewables" ↦ "example/linked-inputs/non-renewables"
             ]
        , imports: []
        , file: File "linked-outputs/stacked-bar-chart-scatter-plot"
        , inputs: [ "nonRenewables" ]
        }
   , δ_out: multiPlotEntry "stacked-bar-chart" (barChart (barSegment 3 2 neg >>> barSegment 4 1 neg >>> barSegment 4 3 neg))
   , out_expect:
        multiPlotEntry "stacked-bar-chart" (barChart (barSegment 3 2 neg >>> barSegment 4 1 neg >>> barSegment 4 3 neg))
           >>> multiPlotEntry "scatter-plot"
              ( scatterPlot
                   ( scatterPoint 4 (field f_y neg)
                        >>> scatterPoint 6 (field f_y neg)
                   )
              )
   }

linkedOutputs_cases :: Array TestLinkedOutputsSpec
linkedOutputs_cases =
   [ { spec:
          { datasets: [ "data" ↦ "example/linked-outputs/pairs-data" ]
          , imports: []
          , file: File "linked-outputs/pairs"
          , inputs: [ "data" ]
          }
     , δ_out: snd (snd (fst neg))
     , out_expect: snd (snd (fst neg))
          >>> fst (fst neg >>> snd (fst neg))
     }
   , { spec:
          { datasets: [ "data" ↦ "example/linked-outputs/convolution-data" ]
          , imports: [ "lib/convolution" ]
          , file: File "linked-outputs/convolution"
          , inputs: [ "data" ]
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
   ]
