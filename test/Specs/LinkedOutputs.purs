module Test.Specs.LinkedOutputs where

import Prelude

import App.Util.Selector (constrArg, dictVal, field, listElement, matrixElement)
import Bind ((↦))
import DataType (cBarChart, cLineChart, cLinePlot, cMultiPlot, cPair, f_bars, f_data, f_plots, f_y, f_z)
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
        (dictVal "bar-chart" (constrArg cBarChart 0 (field f_data (listElement 1 (field f_bars (listElement 0 (field f_z neg)))))))
   , out_expect: constrArg cMultiPlot 0
        ( dictVal "bar-chart" (constrArg cBarChart 0 (field f_data (listElement 1 (field f_bars (listElement 0 (field f_z neg))))))
             >>> dictVal "line-chart"
                ( constrArg cLineChart 0
                     ( field f_plots
                          ( listElement 0 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                               >>> listElement 1 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                               >>> listElement 2 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                               >>> listElement 3 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                          )
                     )
                )
        )
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
     , δ_out: constrArg cPair 1 (constrArg cPair 1 (constrArg cPair 0 neg))
     , out_expect: constrArg cPair 1 (constrArg cPair 1 (constrArg cPair 0 neg))
          >>> constrArg cPair 0 (constrArg cPair 0 neg >>> constrArg cPair 1 (constrArg cPair 0 neg))
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
   ]
