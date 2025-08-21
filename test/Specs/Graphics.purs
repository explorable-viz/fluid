module Test.Specs.Graphics where

import Bind ((↦))

import Test.Util.Suite (TestWithDatasetSpec)

graphics_cases :: Array TestWithDatasetSpec
graphics_cases =
   [ { dataset: "data" ↦ "dataset/renewables-restricted.fld"
     , file: "graphics/background.fld"
     }
   , { dataset: "data" ↦ "dataset/renewables-restricted.fld"
     , file: "graphics/grouped-bar-chart.fld"
     }
   , { dataset: "data" ↦ "dataset/renewables-restricted.fld"
     , file: "graphics/line-chart.fld"
     }
   , { dataset: "data" ↦ "dataset/renewables-restricted.fld"
     , file: "graphics/stacked-bar-chart.fld"
     }
   ]
