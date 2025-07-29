module Test.Specs.Graphics where

import Bind ((↦))

import Test.Util.Suite (TestWithDatasetSpec)

graphics_cases :: Array TestWithDatasetSpec
graphics_cases =
   [ { imports: []
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/background"
     }
   , { imports: []
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/grouped-bar-chart"
     }
   , { imports: []
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/line-chart"
     }
   , { imports: []
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/stacked-bar-chart"
     }
   ]
