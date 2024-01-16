module Test.Specs.Graphics where

import Bind ((↦))

import Test.Util.Suite (TestWithDatasetSpec)

graphics_cases :: Array TestWithDatasetSpec
graphics_cases =
   [ { imports: [ "lib/graphics" ]
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/background"
     }
   , { imports: [ "lib/graphics" ]
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/grouped-bar-chart"
     }
   , { imports: [ "lib/graphics" ]
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/line-chart"
     }
   , { imports: [ "lib/graphics" ]
     , dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/stacked-bar-chart"
     }
   ]
