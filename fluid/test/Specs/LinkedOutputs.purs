module Test.Specs.LinkedOutputs where

import Prelude

import App.Util (SelectionType(..))
import App.Util.Selector (barChart_stackedBars, barSegment, dictVal, fst, lineChart_plots, linePoint, listElement, matrixDims, matrixElement, multiViewEntry, scatterPlot_points, snd, topα, (>.>), select, select')
import Data.Maybe (Maybe(..))
import DataType (f_y)
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
   , δ_out: multiViewEntry 0 (barChart_stackedBars (barSegment 1 0 select))
   , out_expect:
        multiViewEntry 0 (barChart_stackedBars (barSegment 1 0 select))
           >.> multiViewEntry 1
              ( lineChart_plots
                   ( listElement 0 (linePoint 2 (dictVal f_y select))
                        >.> listElement 1 (linePoint 2 (dictVal f_y select))
                        >.> listElement 2 (linePoint 2 (dictVal f_y select))
                        >.> listElement 3 (linePoint 2 (dictVal f_y select))
                   )
              )
   , inert_expect: Nothing
   , file: "slicing/linked_outputs/bar_chart_line_chart.fld"
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
   , δ_out: multiViewEntry 0 (barChart_stackedBars (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
   , out_expect:
        multiViewEntry 0 (barChart_stackedBars (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
           >.> multiViewEntry 1
              ( scatterPlot_points
                   ( listElement 4 (dictVal f_y select)
                        >.> listElement 6 (dictVal f_y select)
                   )
              )
   , inert_expect: Nothing
   , file: "slicing/linked_outputs/stacked_bar_scatter_plot.fld"
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
   , inert_expect: Nothing
   , file: "linked_outputs/moving_average.fld"
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
     , inert_expect: Just (identity >>> (_ × Persistent))
     , file: "linked_outputs/pairs.fld"
     }
   , { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]

          , inputs: [ "data" ]
          , query: Nothing
          , linking: true
          , rowFilter: Nothing
          }
     , δ_out: fst (matrixElement 1 1 select)
     , out_expect:
          fst
             ( matrixElement 1 0 select
                  >.> matrixElement 1 1 select
                  >.> matrixElement 1 2 select
                  >.> matrixElement 1 3 select
                  >.> matrixElement 1 4 select
             )
             >.> snd
                ( matrixElement 0 0 select
                     >.> matrixElement 0 1 select
                     >.> matrixElement 0 2 select
                     >.> matrixElement 1 0 select
                     >.> matrixElement 1 1 select
                     >.> matrixElement 1 2 select
                     >.> matrixElement 2 0 select
                     >.> matrixElement 2 1 select
                     >.> matrixElement 2 2 select
                )
     , inert_expect: Just (topα select' >.> fst (matrixDims select') >.> snd (matrixDims select'))
     , file: "linked_outputs/convolution.fld"
     }
   , linkedOutputs_spec1
   , linkedOutputs_spec2
   , movingAverages_spec
   ]
