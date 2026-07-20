module Test.Specs.LinkedOutputs where

import Prelude

import App.Util (SelectionType(..))
import App.Util.Selector (barSegment, dictVal, fieldElement, listElement, matrixDims, matrixElement, topα, (>.>), select, select')
import Data.Maybe (Maybe(..))
import DataType (cBarChart, cLineChart, cLinePlot, cMultiView, cPair, cScatterPlot, f_fst, f_plots, f_points, f_snd, f_stackedBars, f_views, f_y)
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
   , δ_out: \sels -> fieldElement sels cMultiView f_views 0 (sels cBarChart f_stackedBars (barSegment 1 0 select))
   , out_expect: \sels ->
        fieldElement sels cMultiView f_views 0 (sels cBarChart f_stackedBars (barSegment 1 0 select))
           >.> fieldElement sels cMultiView f_views 1
              ( sels cLineChart f_plots
                   ( listElement 0 (fieldElement sels cLinePlot f_points 2 (dictVal f_y select))
                        >.> listElement 1 (fieldElement sels cLinePlot f_points 2 (dictVal f_y select))
                        >.> listElement 2 (fieldElement sels cLinePlot f_points 2 (dictVal f_y select))
                        >.> listElement 3 (fieldElement sels cLinePlot f_points 2 (dictVal f_y select))
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
   , δ_out: \sels -> fieldElement sels cMultiView f_views 0 (sels cBarChart f_stackedBars (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
   , out_expect: \sels ->
        fieldElement sels cMultiView f_views 0 (sels cBarChart f_stackedBars (barSegment 3 2 select >.> barSegment 4 1 select >.> barSegment 4 3 select))
           >.> fieldElement sels cMultiView f_views 1
              ( sels cScatterPlot f_points
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
     , δ_out: \sels -> sels cPair f_snd select
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
     , δ_out: \sels -> sels cPair f_fst (matrixElement 1 1 select)
     , out_expect: \sels ->
          sels cPair f_fst
             ( matrixElement 1 0 select
                  >.> matrixElement 1 1 select
                  >.> matrixElement 1 2 select
                  >.> matrixElement 1 3 select
                  >.> matrixElement 1 4 select
             )
             >.> sels cPair f_snd
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
     , inert_expect: \sels -> Just (topα select' >.> sels cPair f_fst (matrixDims select') >.> sels cPair f_snd (matrixDims select'))
     , file: "linked_outputs/convolution.fld"
     }
   , linkedOutputs_spec1
   , linkedOutputs_spec2
   , movingAverages_spec
   ]
