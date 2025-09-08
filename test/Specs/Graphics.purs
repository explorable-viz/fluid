module Test.Specs.Graphics where

import Bind ((↦))

import Test.Util.Suite (TestWithDatasetSpec)

graphics_cases :: Array TestWithDatasetSpec
graphics_cases =
   [ { dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/background"
     }
   , { dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/grouped-bar-chart"
     }
   , { dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/line-chart"
     }
   , { dataset: "data" ↦ "dataset/renewables-restricted"
     , file: "graphics/stacked-bar-chart"
     }
   ]
