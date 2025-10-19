module Test.Specs.LinkedOutputs where

import Prelude

import App.Util (SelectionType(..))
import App.Util.Selector (barChart, barSegment, dictVal, fst, lineChart, linePoint, listElement, matrixElement, multiViewEntry, scatterPlot, scatterPoint, snd, (>.>), select)
import Data.Maybe (Maybe(..))
import DataType (f_plots, f_y)
import File (Folder(..))
import Test.Util.Suite (TestLinkedOutputsSpec)
import Util ((×))

linkedOutputs_spec1 :: TestLinkedOutputsSpec
linkedOutputs_spec1 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , inputs: [ "renewables" ]
        , query: Nothing
        , linking: true
        , rowFilter: Nothing
        }
   , δ_out: multiViewEntry 0 (barChart (barSegment 1 0 select))
   , out_expect:
        multiViewEntry 0 (barChart (barSegment 1 0 select))
           >.> multiViewEntry 1
              ( lineChart
                   ( dictVal f_plots
                        ( listElement 0 (linePoint 2 (dictVal f_y select))
                             >.> listElement 1 (linePoint 2 (dictVal f_y select))
                             >.> listElement 2 (linePoint 2 (dictVal f_y select))
                             >.> listElement 3 (linePoint 2 (dictVal f_y select))

                        )
                   )
              )
   , file: "slicing/linkedOutputs/bar-chart-line-chart.fld"
   }

linkedOutputs_spec2 :: TestLinkedOutputsSpec
linkedOutputs_spec2 =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , inputs: [ "nonRenewables" ]
        , query: Nothing
        , linking: true
        , rowFilter: Nothing
        }
   , δ_out: multiViewEntry 0 (barChart (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
   , out_expect:
        multiViewEntry 0 (barChart (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
           >.> multiViewEntry 1
              ( scatterPlot
                   ( scatterPoint 4 (dictVal f_y select)
                        >.> scatterPoint 6 (dictVal f_y select)
                   )
              )
   , file: "slicing/linkedOutputs/stacked-bar-scatter-plot.fld"
   }

movingAverages_spec :: TestLinkedOutputsSpec
movingAverages_spec =
   { spec:
        { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
        , inputs: [ "methane" ]
        , query: Nothing
        , linking: true
        , rowFilter: Nothing
        }
   , δ_out: identity >>> (_ × Persistent) -- TODO: make this a non-trivial test
   , out_expect: identity >>> (_ × Persistent)
   , file: "linkedOutputs/moving-average.fld"
   }

linkedOutputs_cases :: Array TestLinkedOutputsSpec
linkedOutputs_cases =
   [ { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]
          , inputs: [ "data" ]
          , query: Nothing
          , linking: true
          , rowFilter: Nothing
          }
     , δ_out: snd select
     , out_expect: select
     , file: "linkedOutputs/pairs.fld"
     }
   , { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]

          , inputs: [ "data" ]
          , query: Nothing
          , linking: true
          , rowFilter: Nothing
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
     , file: "linkedOutputs/convolution.fld"
     }
   , linkedOutputs_spec1
   , linkedOutputs_spec2
   , movingAverages_spec
   ]
