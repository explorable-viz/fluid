module Test.Specs.Bwd where

import Prelude

import App.Util.Selector (barChart, barSegment, dict, dictKey, dictVal, listCell, listElement, matrixElement, multiPlotEntry, snd, some)
import Bind ((↦))
import Lattice (neg)
import Test.Util.Suite (TestBwdSpec)

bwd_cases :: Array TestBwdSpec
bwd_cases =
   [ { file: "add", imports: [], bwd_expect_file: "add.expect", δv: neg, fwd_expect: "⸨8⸩", datasets: [] }
   , { file: "array/lookup", imports: [], bwd_expect_file: "array/lookup.expect", δv: neg, fwd_expect: "⸨14⸩", datasets: [] }
   , { file: "array/dims", imports: [], bwd_expect_file: "array/dims.expect", δv: neg, fwd_expect: "⸨(⸨3⸩, ⸨3⸩)⸩", datasets: [] }
   , { file: "convolution/edgeDetect"
     , imports:
          [ "lib/convolution"
          , "example/slicing/convolution/filter/edge-detect"
          , "example/slicing/convolution/test-image"
          ]
     , bwd_expect_file: "convolution/edgeDetect.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "⸨0⸩, -1, 2, 0, -1,\n\
          \0, 3, -2, 3, -2,\n\
          \-1, 1, -5, 0, 4,\n\
          \1, -1, 4, 0, -4,\n\
          \1, 0, -3, 2, 0"
     , datasets: []
     }
   , { file: "convolution/emboss"
     , imports:
          [ "lib/convolution"
          , "example/slicing/convolution/filter/emboss"
          , "example/slicing/convolution/test-image"
          ]
     , bwd_expect_file: "convolution/emboss.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "⸨5⸩, 4, 2, 5, 2,\n\
          \3, 1, 2, -1, -2,\n\
          \3, 0, 1, 0, -1,\n\
          \2, 1, -2, 0, 0,\n\
          \1, 0, -1, -1, -2"
     , datasets: []
     }
   , { file: "convolution/gaussian"
     , imports:
          [ "lib/convolution"
          , "example/slicing/convolution/filter/gaussian"
          , "example/slicing/convolution/test-image"
          ]
     , bwd_expect_file: "convolution/gaussian.expect"
     , δv: matrixElement 1 1 neg
     , fwd_expect:
          "⸨38⸩, 37, 28, 30, 38,\n\
          \38, 36, 46, 31, 34,\n\
          \37, 41, 54, 34, 20,\n\
          \21, 35, 31, 31, 42,\n\
          \13, 32, 35, 19, 26"
     , datasets: []
     }
   , { file: "dict/create"
     , imports: []
     , bwd_expect_file: "dict/create.expect"
     , δv: dictKey "ab" neg
     , fwd_expect: "{|\"a\" := 5, ⸨\"ab\"⸩ := 6|}"
     , datasets: []
     }
   , { file: "dict/difference"
     , imports: []
     , bwd_expect_file: "dict/difference.expect"
     , δv: dict neg
     , fwd_expect: "⸨{|\"a\" := 5|}⸩"
     , datasets: []
     }
   , { file: "dict/disjointUnion"
     , imports: []
     , bwd_expect_file: "dict/disjointUnion.expect"
     , δv: dictKey "a" neg >>> dictVal "c" neg
     , fwd_expect: "{|⸨\"a\"⸩ := 5, \"b\" := 6, \"c\" := ⸨7⸩|}"
     , datasets: []
     }
   , { file: "dict/foldl", imports: [], bwd_expect_file: "dict/foldl.expect", δv: neg, fwd_expect: "⸨0⸩", datasets: [] }
   , { file: "dict/intersectionWith"
     , imports: []
     , bwd_expect_file: "dict/intersectionWith.expect"
     , δv: dictVal "b" neg >>> dictVal "c" neg
     , fwd_expect: "{|\"b\" := ⸨0⸩, \"c\" := ⸨20⸩|}"
     , datasets: []
     }
   , { file: "dict/fromRecord"
     , imports: []
     , bwd_expect_file: "dict/fromRecord.expect"
     , δv: dictKey "ab" neg
     , fwd_expect: "⸨{|⸨\"a\"⸩ := 5, ⸨\"ab\"⸩ := 6|}⸩"
     , datasets: []
     }
   , { file: "dict/get", imports: [], bwd_expect_file: "dict/get.expect", δv: neg, fwd_expect: "⸨0⸩", datasets: [] }
   , { file: "dict/map", imports: [], bwd_expect_file: "dict/map.expect", δv: neg, fwd_expect: "⸨20⸩", datasets: [] }
   , { file: "divide"
     , imports: []
     , bwd_expect_file: "divide.expect"
     , δv: neg
     , fwd_expect: "⸨40.22222222222222⸩"
     , datasets: []
     }
   , { file: "dtw/compute-dtw"
     , imports:
          [ "lib/fnum"
          , "lib/dtw"
          ]
     , bwd_expect_file: "dtw/compute-dtw.expect"
     , fwd_expect: "((1, 1) : (⸨(⸨2⸩, ⸨2⸩)⸩ : ((2, 3) : ((3, 4) : ((4, 5) : ((5, 6) : ((5, 7) : [])))))))"
     , δv: listElement 1 neg
     , datasets: []
     }
   , { file: "dtw/average-series"
     , imports:
          [ "lib/fnum"
          , "lib/dtw"
          ]
     , bwd_expect_file: "dtw/average-series.expect"
     , fwd_expect: "(2.5 : (0.5 : (⸨0.5⸩ : (2.5 : (2.5 : (1.0 : (0.5 : [])))))))"
     , δv: listElement 2 neg
     , datasets: []
     }
   , { file: "filter"
     , imports: []
     , bwd_expect_file: "filter.expect"
     , δv: listCell 0 neg
     , fwd_expect: "⸨(⸨8⸩ : (7 : []))⸩"
     , datasets: []
     }
   , { file: "intersperse"
     , imports: []
     , bwd_expect_file: "intersperse-1.expect"
     , δv: listCell 1 neg
     , fwd_expect: "(1 : ⸨(0 : (2 : (0 : (3 : []))))⸩)"
     , datasets: []
     }
   , { file: "intersperse"
     , imports: []
     , bwd_expect_file: "intersperse-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "⸨(1 : (0 : ⸨(2 : (0 : (3 : [])))⸩))⸩"
     , datasets: []
     }
   , { file: "length", imports: [], bwd_expect_file: "length.expect", δv: neg, fwd_expect: "⸨5⸩", datasets: [] }
   , { file: "list-comp"
     , imports: []
     , bwd_expect_file: "list-comp-1.expect"
     , δv: listCell 1 neg
     , fwd_expect: "(6.2 : ⸨(260 : (19.9 : (91 : [])))⸩)"
     , datasets: []
     }
   , { file: "list-comp"
     , imports: []
     , bwd_expect_file: "list-comp-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(6.2 : (260 : ⸨(19.9 : (91 : []))⸩))"
     , datasets: []
     }
   , { file: "lookup"
     , imports: []
     , bwd_expect_file: "lookup.expect"
     , δv: some neg
     , fwd_expect: "⸨Some \"Germany\"⸩"
     , datasets: []
     }
   , { file: "map"
     , imports: []
     , bwd_expect_file: "map.expect"
     , δv: listCell 0 neg >>> listCell 1 neg
     , fwd_expect: "⸨(5 : ⸨(6 : [])⸩)⸩"
     , datasets: []
     }
   , { file: "matrix-update"
     , imports: []
     , bwd_expect_file: "matrix-update.expect"
     , fwd_expect:
          "15, 13, 6, 9, 16,\n\
          \12, ⸨4000⸩, 15, 4, 13,\n\
          \14, 9, 20, 8, 1,\n\
          \4, 10, 3, 7, 19,\n\
          \3, 11, 15, 2, 9"
     , δv: matrixElement 2 2 neg
     , datasets: []
     }
   , { file: "multiply", imports: [], bwd_expect_file: "multiply.expect", δv: neg, fwd_expect: "⸨0⸩", datasets: [] }
   , { file: "nth", imports: [], bwd_expect_file: "nth.expect", δv: neg, fwd_expect: "⸨4⸩", datasets: [] }
   , { file: "output-not-source"
     , imports: []
     , bwd_expect_file: "output-not-source.expect"
     , fwd_expect: "(⸨3⸩, ⸨True⸩)"
     , δv: snd neg -- selection on just first component will be discarded by bwdSlice; see #818.
     , datasets: []
     }
   , { file: "section-5-example"
     , imports: []
     , bwd_expect_file: "section-5-example-1.expect"
     , δv: listCell 0 neg
     , fwd_expect: "⸨(88 : (6 : (4 : [])))⸩"
     , datasets: []
     }
   , { file: "section-5-example"
     , imports: []
     , bwd_expect_file: "section-5-example-2.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(⸨88⸩ : (⸨6⸩ : (⸨4⸩ : [])))"
     , datasets: []
     }
   , { file: "section-5-example"
     , imports: []
     , bwd_expect_file: "section-5-example-3.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(88 : (6 : ⸨(4 : [])⸩))"
     , datasets: []
     }
   , { file: "zeros"
     , imports: []
     , bwd_expect_file: "zeros-1.expect"
     , δv: listCell 0 neg >>> listCell 2 neg
     , fwd_expect: "⸨(0 : (0 : ⸨[]⸩))⸩"
     , datasets: []
     }
   , { file: "zeros"
     , imports: []
     , bwd_expect_file: "zeros-2.expect"
     , δv: listCell 2 neg
     , fwd_expect: "(0 : (0 : ⸨[]⸩))"
     , datasets: []
     }
   , { file: "zipWith"
     , imports: []
     , bwd_expect_file: "zipWith-1.expect"
     , δv: listElement 1 neg
     , fwd_expect: "(13.0 : (⸨25.0⸩ : (41.0 : [])))"
     , datasets: []
     }
   , { file: "linked-outputs/bar-chart-line-chart"
     , imports: []
     , bwd_expect_file: "linked-outputs/bar-chart-line-chart.expect"
     , δv: multiPlotEntry "bar-chart" (barChart (barSegment 1 0))
     , fwd_expect: "MultiPlot {|\"bar-chart\" := BarChart {caption : \"Total output by country\", data : ({bars : ({y : \"output\", z : 295.3} : []), x : \"China\"} : ({bars : ({y : \"output\", z : ⸨196.7⸩} : []), x : \"USA\"} : ({bars : ({y : \"output\", z : 97.69999999999999} : []), x : \"Germany\"} : [])))}, \"line-chart\" := LineChart {caption : \"Output of USA relative to China\", plots : (LinePlot {data : ({x : 2013, y : 2.5483870967741935} : ({x : 2014, y : 1.61} : ({x : 2015, y : 1.6213592233009706} : ({x : 2016, y : 1.4000000000000001} : ({x : 2017, y : 1.1208053691275166} : ({x : 2018, y : 0.9101123595505617} : [])))))), name : \"Bio\"} : (LinePlot {data : ({x : 2013, y : 0.3} : ({x : 2014, y : 0.28214285714285714} : ({x : 2015, y : 0.8333333333333334} : ({x : 2016, y : 0.26229508196721313} : ({x : 2017, y : 0.25559105431309903} : ({x : 2018, y : 0.2484472049689441} : [])))))), name : \"Hydro\"} : (LinePlot {data : ({x : 2013, y : 0.6080402010050252} : ({x : 2014, y : 0.6428571428571429} : ({x : 2015, y : 0.5909090909090909} : ({x : 2016, y : 0.5324675324675324} : ({x : 2017, y : 0.3893129770992366} : ({x : 2018, y : 0.3522727272727273} : [])))))), name : \"Solar\"} : (LinePlot {data : ({x : 2013, y : 0.6703296703296703} : ({x : 2014, y : 0.5739130434782609} : ({x : 2015, y : 0.5103448275862069} : ({x : 2016, y : 0.48520710059171596} : ({x : 2017, y : 0.4734042553191489} : ({x : 2018, y : 0.45714285714285713} : [])))))), name : \"Wind\"} : []))))}|}"
     , datasets: [ "renewables" ↦ "example/linked-outputs/renewables" ]
     }
   , { file: "linked-outputs/stacked-bar-chart-scatter-plot"
     , imports: []
     , bwd_expect_file: "linked-outputs/stacked-bar-chart-scatter-plot.expect"
     , δv: multiPlotEntry "stacked-bar-chart" (barChart (barSegment 3 2 >>> barSegment 4 1 >>> barSegment 4 3))
     , fwd_expect: "MultiPlot {|\"scatter-plot\" := ScatterPlot {caption : \"Clean energy efficiency vs proportion of renewable energy capacity\", data : ({x : 0.8723185510332055, y : 0.4180741155728385} : ({x : 0.383891020964826, y : 0.3306374135311273} : ({x : 0.5685559399722339, y : 0.2651713517303818} : ({x : 0.39179907463864283, y : 0.5311676111397315} : ({x : 0.0886691179578209, y : 0.4125357483317445} : ({x : 0.3167847396421975, y : 0.2767379556904734} : ({x : 0.3129857171819161, y : 0.20426921772653447} : ({x : 0.29687029792356306, y : 0.3462200657379872} : ({x : 0.16239390265026848, y : 0.4128} : ({x : 0.2115752867627615, y : 0.5086651868096602} : [])))))))))), xlabel : \"Renewables/TotalEnergyCap\", ylabel : \"Clean Capacity Factor\"}, \"stacked-bar-chart\" := BarChart {caption : \"Non-renewables by country\", data : ({bars : ({y : \"BRA\", z : 151.05} : ({y : \"EGY\", z : 159.93} : ({y : \"IND\", z : 1060.1799999999998} : ({y : \"JPN\", z : 928.82} : [])))), x : \"2014\"} : ({bars : ({y : \"BRA\", z : 142.76} : ({y : \"EGY\", z : 170.68} : ({y : \"IND\", z : 1118.8899999999999} : ({y : \"JPN\", z : 876.0999999999999} : [])))), x : \"2015\"} : ({bars : ({y : \"BRA\", z : 108.03} : ({y : \"EGY\", z : 174.07999999999998} : ({y : \"IND\", z : 1193.53} : ({y : \"JPN\", z : 883.3299999999999} : [])))), x : \"2016\"} : ({bars : ({y : \"BRA\", z : 116.76} : ({y : \"EGY\", z : 181.31} : ({y : \"IND\", z : ⸨1236.43⸩} : ({y : \"JPN\", z : 875.32} : [])))), x : \"2017\"} : ({bars : ({y : \"BRA\", z : 101.48} : ({y : \"EGY\", z : ⸨182.31⸩} : ({y : \"IND\", z : 1315.57} : ({y : \"JPN\", z : ⸨873.39⸩} : [])))), x : \"2018\"} : [])))))}|}"
     , datasets:
          [ "renewables" ↦ "example/linked-inputs/renewables"
          , "nonRenewables" ↦ "example/linked-inputs/non-renewables"
          ]
     }
   ]
