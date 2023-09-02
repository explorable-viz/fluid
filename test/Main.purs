module Test.Main where

import Prelude hiding (add)
import App.Util (as𝔹Selector, selectBarChart_data, selectMatrixElement, selectNth, selectNthCell, selectPair, selectSome, select_y)
import Bindings ((↦))
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Set as S
import Data.Traversable (traverse_)
import Dict (fromFoldable) as D
import Effect (Effect)
import Effect.Console (log, logShow)
import Graph (Vertex(..), inEdges)
import Graph (fromFoldable) as G
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (fwdSlice)
import Lattice (botOf, neg, topOf)
import Module (File(..))
import Test.Util (Test, run, testWithDatasetMany, testLinkMany, testMany, testBwdMany)
import Util ((×))
import Val (DictRep(..), Val(..))

main :: Effect Unit
main = do
   traverse_ run $ tests false

tests :: Boolean -> Array (Test Unit)
tests is_bench =
   [ test_desugaring is_bench
   , test_misc is_bench
   , test_bwd is_bench
   , test_graphics is_bench
   , test_linking
   , test_graph
   ]

{-
tests = [ test_scratchpad ]
-}

test_scratchpad :: Boolean -> Test Unit
test_scratchpad = testBwdMany
   [ (File "filter") × (File "filter.expect") × (botOf >>> selectNthCell 0 neg) × "(_8_ _:_ (7 : []))"
   ]

test_desugaring :: Boolean -> Test Unit
test_desugaring = testMany
   [ (File "desugar/list-comp-1") × "(14 : (12 : (10 : (13 : (11 : (9 : (12 : (10 : (8 : [])))))))))"
   , (File "desugar/list-comp-2") ×
        "(14 : (14 : (14 : (12 : (12 : (12 : (10 : (10 : (10 : (13 : (13 : (13 : (11 : (11 : (11 : (9 : \
        \(9 : (9 : (12 : (12 : (12 : (10 : (10 : (10 : (8 : (8 : (8 : [])))))))))))))))))))))))))))"
   , (File "desugar/list-comp-3") × "(9 : (8 : []))"
   , (File "desugar/list-comp-4") × "(5 : (4 : (3 : [])))"
   , (File "desugar/list-comp-5") × "(5 : (4 : (3 : [])))"
   , (File "desugar/list-comp-6") × "(5 : [])"
   , (File "desugar/list-comp-7") × "([] : [])"
   , (File "desugar/list-enum") × "(3 : (4 : (5 : (6 : (7 : [])))))"
   ]

test_misc :: Boolean -> Test Unit
test_misc = testMany
   [ (File "arithmetic") × "42"
   , (File "array") × "(1, (3, 3))"
   , (File "compose") × "5"
   , (File "dicts") × "{d: {||}, e: {|\"a\":= 5, \"ab\":= 6|}, e_ab: 6, f: {|\"a\":= 6, \"ab\":= 7|}, g: {|\"a\":= 5|}, h: {|\"fst\":= 4, \"snd\":= (6 : (7 : []))|}}"
   , (File "div-mod-quot-rem") ×
        "((1 : (-1 : (-2 : (2 : [])))) : \
        \((2 : (2 : (1 : (1 : [])))) : \
        \((1 : (-1 : (-1 : (1 : [])))) : \
        \((2 : (2 : (-2 : (-2 : [])))) : []))))"
   , (File "factorial") × "40320"
   , (File "filter") × "(8 : (7 : []))"
   , (File "first-class-constr") × "((10 : []) : ((12 : []) : ((20 : []) : [])))"
   , (File "flatten") × "((3, \"simon\") : ((4, \"john\") : ((6, \"sarah\") : ((7, \"claire\") : []))))"
   , (File "foldr_sumSquares") × "661"
   , (File "lexicalScoping") × "\"6\""
   , (File "length") × "2"
   , (File "lookup") × "Some \"sarah\""
   , (File "map") × "(5 : (7 : (13 : (15 : (4 : (3 : (-3 : [])))))))"
   , (File "mergeSort") × "(1 : (2 : (3 : [])))"
   , (File "normalise") × "(33, 66)"
   , (File "pattern-match") × "4"
   , (File "range") × "((0, 0) : ((0, 1) : ((1, 0) : ((1, 1) : []))))"
   , (File "records") × "{a: 2, b: 6, c: 7, d: (5 : []), e: 7}"
   , (File "reverse") × "(2 : (1 : []))"
   ]

test_bwd :: Boolean -> Test Unit
test_bwd = testBwdMany
   [ (File "add") × (File "add.expect") × (const $ Int true 8) × "_8_"
   , (File "array/lookup") × (File "array/lookup.expect") × (const $ Int true 14) × "_14_"
   , (File "array/dims") × (File "array/dims.expect") × topOf × "_(_3_, _3_)_"
   , (File "convolution/edgeDetect") × (File "convolution/edgeDetect.expect")
        × (selectMatrixElement 1 1 # as𝔹Selector)
        ×
           "_0_, -1, 2, 0, -1,\n\
           \0, 3, -2, 3, -2,\n\
           \-1, 1, -5, 0, 4,\n\
           \1, -1, 4, 0, -4,\n\
           \1, 0, -3, 2, 0"
   , (File "convolution/emboss") × (File "convolution/emboss.expect")
        × (selectMatrixElement 1 1 # as𝔹Selector)
        ×
           "_5_, 4, 2, 5, 2,\n\
           \3, 1, 2, -1, -2,\n\
           \3, 0, 1, 0, -1,\n\
           \2, 1, -2, 0, 0,\n\
           \1, 0, -1, -1, -2"
   , (File "convolution/gaussian") × (File "convolution/gaussian.expect")
        × (selectMatrixElement 1 1 # as𝔹Selector)
        ×
           "_38_, 37, 28, 30, 38,\n\
           \38, 36, 46, 31, 34,\n\
           \37, 41, 54, 34, 20,\n\
           \21, 35, 31, 31, 42,\n\
           \13, 32, 35, 19, 26"
   , (File "dict/create") × (File "dict/create.expect")
        ×
           ( const $ Dictionary false $ DictRep $ D.fromFoldable
                [ "a" ↦ (false × Int false 5)
                , "ab" ↦ (true × Int false 6)
                ]
           )
        ×
           "{|\"a\":= 5, _\"ab\"_:= 6|}"
   , (File "dict/difference") × (File "dict/difference.expect")
        ×
           ( const $ Dictionary true $ DictRep $ D.fromFoldable
                [ "a" ↦ (false × Int false 5)
                ]
           )
        ×
           "_{|\"a\":= 5|}_"
   , (File "dict/disjointUnion") × (File "dict/disjointUnion.expect")
        ×
           ( const $ Dictionary false $ DictRep $ D.fromFoldable
                [ "a" ↦ (true × Int false 5)
                , "b" ↦ (false × Int false 6)
                , "c" ↦ (false × Int true 7)
                ]
           )
        ×
           "{|_\"a\"_:= 5, \"b\":= 6, \"c\":= _7_|}"
   , (File "dict/foldl") × (File "dict/foldl.expect")
        × topOf
        ×
           "_0_"
   , (File "dict/intersectionWith") × (File "dict/intersectionWith.expect")
        ×
           ( const $ Dictionary false $ DictRep $ D.fromFoldable
                [ "b" ↦ (false × Int true 0)
                , "c" ↦ (false × Int true 20)
                ]
           )
        ×
           "{|\"b\":= _0_, \"c\":= _20_|}"
   , (File "dict/fromRecord") × (File "dict/fromRecord.expect")
        ×
           ( const $ Dictionary false $ DictRep $ D.fromFoldable
                [ "a" ↦ (false × Int false 5)
                , "ab" ↦ (true × Int false 6)
                ]
           )
        ×
           "_{|_\"a\"_:= 5, _\"ab\"_:= 6|}_"
   , (File "dict/get") × (File "dict/get.expect")
        × (const $ Int true 0)
        ×
           "_0_"
   , (File "dict/map") × (File "dict/map.expect")
        × (const $ Int true 20)
        ×
           "_20_"
   , (File "divide") × (File "divide.expect") × topOf × "_40.22222222222222_"
   , (File "filter") × (File "filter.expect") × (botOf >>> selectNthCell 0 neg) × "(_8_ _:_ (7 : []))"
   , (File "intersperse") × (File "intersperse-1.expect") × (botOf >>> selectNthCell 1 neg) ×
        "(1 : (0 _:_ (2 : (0 : (3 : [])))))"
   , (File "intersperse") × (File "intersperse-2.expect") × (botOf >>> selectNthCell 2 neg) ×
        "(1 _:_ (0 : (2 _:_ (0 : (3 : [])))))"
   , (File "length") × (File "length.expect") × topOf × "_5_"
   , (File "list-comp") × (File "list-comp-1.expect") × (botOf >>> selectNthCell 1 neg) ×
        "(6.2 : (260 _:_ (19.9 : (91 : []))))"
   , (File "list-comp") × (File "list-comp-2.expect") × (botOf >>> selectNthCell 2 neg) ×
        "(6.2 : (260 : (19.9 _:_ (91 : []))))"
   , (File "lookup") × (File "lookup.expect") × (selectSome # as𝔹Selector) × "_Some_ \"Germany\""
   , (File "map") × (File "map.expect") × (botOf >>> selectNthCell 0 neg >>> selectNthCell 1 neg) ×
        "(5 _:_ (6 _:_ []))"
   , (File "multiply") × (File "multiply.expect") × (const $ Int true 0) × "_0_"
   , (File "nth") × (File "nth.expect") × (const $ Int true 4) × "_4_"
   , (File "section-5-example") × (File "section-5-example-1.expect") × (botOf >>> selectNthCell 0 neg) ×
        "(88 _:_ (6 : (4 : [])))"
   , (File "section-5-example") × (File "section-5-example-2.expect") × (botOf >>> selectNth 1 topOf) ×
        "(_88_ : (_6_ : (_4_ : [])))"
   , (File "section-5-example") × (File "section-5-example-3.expect") × (botOf >>> selectNthCell 2 neg) ×
        "(88 : (6 : (4 _:_ [])))"
   , (File "zeros") × (File "zeros-1.expect") × (botOf >>> selectNthCell 0 neg >>> selectNthCell 2 neg) ×
        "(0 _:_ (0 : _[]_))"
   , (File "zeros") × (File "zeros-2.expect") × (botOf >>> selectNthCell 2 neg) × "(0 : (0 : _[]_))"
   , (File "zipWith") × (File "zipWith-1.expect")
        × (botOf >>> selectNth 1 (const $ Float true 25.0))
        ×
           "(13.0 : (_25.0_ : (41.0 : [])))"
   ]

test_graphics :: Boolean -> Test Unit
test_graphics = testWithDatasetMany
   [ (File "dataset/renewables-restricted") × (File "graphics/background")
   , (File "dataset/renewables-restricted") × (File "graphics/grouped-bar-chart")
   , (File "dataset/renewables-restricted") × (File "graphics/line-chart")
   , (File "dataset/renewables-restricted") × (File "graphics/stacked-bar-chart")
   ]

test_linking :: Test Unit
test_linking = testLinkMany
   [ { divId: ""
     , file1: File "pairs-1"
     , file2: File "pairs-2"
     , dataFile: File "pairs-data"
     , x: "data"
     }
        ×
           ( selectPair (const false) botOf
                ( selectPair (const false) botOf
                     (selectPair (const false) (const $ Int true 3) botOf)
                )
           )
        × "(3, (_5_, _7_))"
   , { divId: ""
     , file1: File "convolution-1"
     , file2: File "convolution-2"
     , dataFile: File "convolution-data"
     , x: "data"
     }
        × (selectMatrixElement 2 2 # as𝔹Selector)
        ×
           "_18_, _12_, _13_, 9, 19,\n\
           \_20_, _11_, _24_, 9, 14,\n\
           \_15_, _13_, _20_, 11, 14,\n\
           \7, 15, 15, 8, 20,\n\
           \3, 10, 12, 3, 11"
   , { divId: ""
     , file1: File "bar-chart"
     , file2: File "line-chart"
     , dataFile: File "renewables"
     , x: "data"
     }
        × (botOf >>> selectBarChart_data (selectNth 1 (select_y topOf)))
        ×
           "LineChart ({\
           \caption: \"Output of USA relative to China\", \
           \plots: \
           \(LinePlot ({\
           \data: \
           \({x: 2013, y: 2.5483870967741935} : \
           \({x: 2014, y: 1.61} : \
           \({x: 2015, y: _1.6213592233009706_} : \
           \({x: 2016, y: 1.4000000000000001} : \
           \({x: 2017, y: 1.1208053691275166} : \
           \({x: 2018, y: 0.9101123595505617} : [])))))), \
           \name: \"Bio\"\
           \}) : \
           \(LinePlot ({\
           \data: \
           \({x: 2013, y: 0.3} : \
           \({x: 2014, y: 0.28214285714285714} : \
           \({x: 2015, y: _0.8333333333333334_} : \
           \({x: 2016, y: 0.26229508196721313} : \
           \({x: 2017, y: 0.25559105431309903} : \
           \({x: 2018, y: 0.2484472049689441} : [])))))), \
           \name: \"Hydro\"\
           \}) : \
           \(LinePlot ({\
           \data: \
           \({x: 2013, y: 0.6080402010050252} : \
           \({x: 2014, y: 0.6428571428571429} : \
           \({x: 2015, y: _0.5909090909090909_} : \
           \({x: 2016, y: 0.5324675324675324} : \
           \({x: 2017, y: 0.3893129770992366} : \
           \({x: 2018, y: 0.3522727272727273} : [])))))), \
           \name: \"Solar\"\
           \}) : \
           \(LinePlot ({\
           \data: ({x: 2013, y: 0.6703296703296703} : \
           \({x: 2014, y: 0.5739130434782609} : \
           \({x: 2015, y: _0.5103448275862069_} : \
           \({x: 2016, y: 0.48520710059171596} : \
           \({x: 2017, y: 0.4734042553191489} : \
           \({x: 2018, y: 0.45714285714285713} : [])))))), \
           \name: \"Wind\"\
           \}) : []))))\
           \})"
   ]

-- Remove once graph slicing tested as part of overall infrastructure.
test_graph :: Test Unit
test_graph = do
   let
      ids = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
      adds = foldl
         ( \acc α ->
              (Vertex (show α) × S.fromFoldable [ Vertex (show (α + 2)), Vertex (show (α + 3)) ]) : acc
         )
         Nil
         ids
      g' = G.fromFoldable adds :: GraphImpl (S.Set)
      slice = fwdSlice (S.fromFoldable [ Vertex "13", Vertex "12", Vertex "11" ]) g'
   lift $ do
      log ("Outedges: " <> show (inEdges g' (S.fromFoldable [ Vertex "11" ])))
      logShow slice
