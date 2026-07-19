module Test.Specs.LinkedOutputs where

import Prelude

import App.Util (SelectionType(..))
import App.Util.Selector (barSegment, dictVal, fst, listElement, matrixDims, matrixElement, snd, topα, (>.>), select, select')
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
   , δ_out: \sels -> sels.multiViewEntry 0 (sels.barChart_stackedBars (barSegment 1 0 select))
   , out_expect: \sels ->
        sels.multiViewEntry 0 (sels.barChart_stackedBars (barSegment 1 0 select))
           >.> sels.multiViewEntry 1
              ( sels.lineChart_plots
                   ( listElement 0 (sels.linePoint 2 (dictVal f_y select))
                        >.> listElement 1 (sels.linePoint 2 (dictVal f_y select))
                        >.> listElement 2 (sels.linePoint 2 (dictVal f_y select))
                        >.> listElement 3 (sels.linePoint 2 (dictVal f_y select))
                   )
              )
   , inert_expect: \_ -> Nothing
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
   , δ_out: \sels -> sels.multiViewEntry 0 (sels.barChart_stackedBars (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
   , out_expect: \sels ->
        sels.multiViewEntry 0 (sels.barChart_stackedBars (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
           >.> sels.multiViewEntry 1
              ( sels.scatterPlot_points
                   ( listElement 4 (dictVal f_y select)
                        >.> listElement 6 (dictVal f_y select)
                   )
              )
   , inert_expect: \_ -> Nothing
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
   , δ_out: \_ -> identity >>> (_ × Persistent) -- TODO: make this a non-trivial test
   , out_expect: \_ -> identity >>> (_ × Persistent)
   , inert_expect: \_ -> Nothing
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
     , δ_out: \_ -> snd select
     , out_expect: \_ -> select
     , inert_expect: \_ -> Just (identity >>> (_ × Persistent))
     , file: "linked_outputs/pairs.fld"
     }
   , { spec:
          { fluidSrcPaths: [ Folder "fluid", Folder "test/fluid" ]

          , inputs: [ "data" ]
          , query: Nothing
          , linking: true
          , rowFilter: Nothing
          }
     , δ_out: \_ -> fst (matrixElement 1 1 select)
     , out_expect: \_ ->
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
     , inert_expect: \_ -> Just (topα select' >.> fst (matrixDims select') >.> snd (matrixDims select'))
     , file: "linked_outputs/convolution.fld"
     }
   , linkedOutputs_spec1
   , linkedOutputs_spec2
   , movingAverages_spec
   ]
