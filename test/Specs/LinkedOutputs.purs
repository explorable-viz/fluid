module Test.Specs.LinkedOutputs where

import Prelude

import App.Util.Selector (barSegment, constrArg, dictVal, field, fst, linePoint, listElement, matrixElement, snd)
import Bind ((↦))
import DataType (cBarChart, cLineChart, cMultiPlot, cPair, f_plots, f_y)
import Lattice (neg)
import Module (File(..))
import Test.Util.Suite (TestLinkedOutputsSpec)

linkedOutputs_spec1 :: TestLinkedOutputsSpec
linkedOutputs_spec1 =
   { spec:
        { divId: "fig-1"
        , datasets: [ "renewables" ↦ "example/linked-outputs/renewables" ]
        , imports: []
        , file: File "linked-outputs/bar-chart-line-chart"
        , inputs: [ "renewables" ]
        }
   , δ_out: constrArg cMultiPlot 0
        (dictVal "bar-chart" (constrArg cBarChart 0 (barSegment 1 0)))
   , out_expect: constrArg cMultiPlot 0
        ( dictVal "bar-chart" (constrArg cBarChart 0 (barSegment 1 0))
             >>> dictVal "line-chart"
                ( constrArg cLineChart 0
                     ( field f_plots
                          ( listElement 0 (linePoint 2 (field f_y neg))
                               >>> listElement 1 (linePoint 2 (field f_y neg))
                               >>> listElement 2 (linePoint 2 (field f_y neg))
                               >>> listElement 3 (linePoint 2 (field f_y neg))
                          )
                     )
                )
        )
   }

linkedOutputs_spec2 :: TestLinkedOutputsSpec
linkedOutputs_spec2 =
   { spec:
        { divId: "fig-1"
        , datasets: [ "nonRenewables" ↦ "example/linked-inputs/non-renewables" ]
        , imports: []
        , file: File "linked-outputs/stacked-bar-chart"
        , inputs: [ "nonRenewables" ]
        }
   , δ_out: constrArg cBarChart 0 (barSegment 3 2 >>> barSegment 4 1 >>> barSegment 4 3)
   , out_expect: constrArg cBarChart 0 (barSegment 3 2 >>> barSegment 4 1 >>> barSegment 4 3)
   }

linkedOutputs_cases :: Array TestLinkedOutputsSpec
linkedOutputs_cases =
   [ { spec:
          { divId: ""
          , datasets: [ "data" ↦ "example/linked-outputs/pairs-data" ]
          , imports: []
          , file: File "linked-outputs/pairs"
          , inputs: [ "data" ]
          }
     , δ_out: snd (snd (fst neg))
     , out_expect: snd (snd (fst neg))
          >>> fst (fst neg >>> snd (fst neg))
     }
   , { spec:
          { divId: ""
          , datasets: [ "data" ↦ "example/linked-outputs/convolution-data" ]
          , imports: [ "lib/convolution" ]
          , file: File "linked-outputs/convolution"
          , inputs: [ "data" ]
          }
     , δ_out: constrArg cPair 0 (matrixElement 2 2 neg)
     , out_expect:
          constrArg cPair 0
             ( matrixElement 2 1 neg
                  >>> matrixElement 2 2 neg
                  >>> matrixElement 2 3 neg
                  >>> matrixElement 2 4 neg
                  >>> matrixElement 2 5 neg
             )
             >>> constrArg cPair 1
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
