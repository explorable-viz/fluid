module PLDI2024 where

import Prelude hiding (absurd)

import App (linkedOutputsFig1)
import App.Fig (drawLinkedOutputsFigs, loadLinkedOutputsFig)
import App.Util.Select (constrArg, field, listElement)
import DataType (cBarChart, f_data, f_y)
import Effect (Effect)
import Lattice (neg)
import Test.Util.Many (TestLinkedOutputsSpec)

-- Tests used to generate figures; each should also be imported into a test suite.
-- (For now just a duplicate of convolution test so not imported into any suites yet.)
convTest :: TestLinkedOutputsSpec
convTest =
   { spec: linkedOutputsFig1
   , δv1: constrArg cBarChart 0
        $ field f_data
        $ listElement 1
        $ field f_y neg
   , v2_expect:
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

main :: Effect Unit
main = do
   drawLinkedOutputsFigs [ loadLinkedOutputsFig convTest.spec ]
