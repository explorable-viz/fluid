module Test.Specs.LinkedOutputs where

import Prelude

import App.Util.Selector (constrArg, dictVal, field, listElement, matrixElement)
import Bind ((↦))
import Data.Either (Either(..))
import DataType (cBarChart, cMultiPlot, cPair, f_data, f_y)
import Lattice (neg)
import Module (File(..))
import Test.Util.Suite (TestLinkedOutputsSpec2, TestLinkedOutputsSpec)

linkedOutputs_spec1' :: TestLinkedOutputsSpec2
linkedOutputs_spec1' =
   { spec:
        { divId: "fig-1"
        , datasets: [ "renewables" ↦ "example/linked-outputs/renewables" ]
        , imports: []
        , file: File "linked-outputs/bar-chart-line-chart"
        , inputs: [ "renewables" ]
        }
   , δ_out: constrArg cMultiPlot 0
        (dictVal "bar chart" (constrArg cBarChart 0 (field f_data (listElement 1 (field f_y neg)))))
   , out_expect: neg
   {-
constrArg cMultiPlot 0
        ( dictVal "line chart"
             ( constrArg cLineChart 0
                  ( field f_plots
                       ( listElement 0 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                            >>> listElement 1 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                            >>> listElement 2 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                            >>> listElement 3 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                            >>> listElement 4 (constrArg cLinePlot 0 (field f_data (listElement 2 (field f_y neg))))
                       )
                  )
             )
        )-}
   }

linkedOutputs_spec1 :: TestLinkedOutputsSpec
linkedOutputs_spec1 =
   { spec:
        { divId: "fig-1"
        , dataFile: File "renewables"
        , imports: []
        , file1: File "bar-chart"
        , file2: File "line-chart"
        , x: "data"
        }
   , δv: Left
        $ constrArg cBarChart 0
        $ field f_data
        $ listElement 1
        $ field f_y neg
   , v'_expect:
        "LineChart {\
        \caption : \"Output of USA relative to China\", \
        \plots : \
        \(LinePlot {\
        \data : \
        \({x : 2013, y : 2.5483870967741935} : \
        \({x : 2014, y : 1.61} : \
        \({x : 2015, y : ⸨1.6213592233009706⸩} : \
        \({x : 2016, y : 1.4000000000000001} : \
        \({x : 2017, y : 1.1208053691275166} : \
        \({x : 2018, y : 0.9101123595505617} : [])))))), \
        \name : \"Bio\"\
        \} : \
        \(LinePlot {\
        \data : \
        \({x : 2013, y : 0.3} : \
        \({x : 2014, y : 0.28214285714285714} : \
        \({x : 2015, y : ⸨0.8333333333333334⸩} : \
        \({x : 2016, y : 0.26229508196721313} : \
        \({x : 2017, y : 0.25559105431309903} : \
        \({x : 2018, y : 0.2484472049689441} : [])))))), \
        \name : \"Hydro\"\
        \} : \
        \(LinePlot {\
        \data : \
        \({x : 2013, y : 0.6080402010050252} : \
        \({x : 2014, y : 0.6428571428571429} : \
        \({x : 2015, y : ⸨0.5909090909090909⸩} : \
        \({x : 2016, y : 0.5324675324675324} : \
        \({x : 2017, y : 0.3893129770992366} : \
        \({x : 2018, y : 0.3522727272727273} : [])))))), \
        \name : \"Solar\"\
        \} : \
        \(LinePlot {\
        \data : ({x : 2013, y : 0.6703296703296703} : \
        \({x : 2014, y : 0.5739130434782609} : \
        \({x : 2015, y : ⸨0.5103448275862069⸩} : \
        \({x : 2016, y : 0.48520710059171596} : \
        \({x : 2017, y : 0.4734042553191489} : \
        \({x : 2018, y : 0.45714285714285713} : [])))))), \
        \name : \"Wind\"\
        \} : []))))\
        \}"
   }

linkedOutputs_cases2 :: Array TestLinkedOutputsSpec2
linkedOutputs_cases2 =
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
   , linkedOutputs_spec1'
   ]

linkedOutputs_cases :: Array TestLinkedOutputsSpec
linkedOutputs_cases =
   [ { spec:
          { divId: ""
          , dataFile: File "pairs-data"
          , imports: []
          , file1: File "pairs-1"
          , file2: File "pairs-2"
          , x: "data"
          }
     , δv: Left
          $ constrArg cPair 1
          $ constrArg cPair 1
          $ constrArg cPair 0 neg
     , v'_expect: "(3, (⸨5⸩, ⸨7⸩))"
     }
   , { spec:
          { divId: ""
          , dataFile: File "convolution-data"
          , imports: [ "lib/convolution" ]
          , file1: File "convolution-1"
          , file2: File "convolution-2"
          , x: "data"
          }
     , δv: Left $ matrixElement 2 2 neg
     , v'_expect:
          "⸨18⸩, ⸨12⸩, ⸨13⸩, 9, 19,\n\
          \⸨20⸩, ⸨11⸩, ⸨24⸩, 9, 14,\n\
          \⸨15⸩, ⸨13⸩, ⸨20⸩, 11, 14,\n\
          \7, 15, 15, 8, 20,\n\
          \3, 10, 12, 3, 11"
     }
   , linkedOutputs_spec1
   ]
