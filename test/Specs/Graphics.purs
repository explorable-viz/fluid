module Test.Specs.Graphics where

import Test.Util.Suite (TestWithDatasetSpec)

graphics_cases :: Array TestWithDatasetSpec
graphics_cases =
   [ { file: "graphics/background.fld" }

   , { file: "graphics/grouped-bar-chart.fld"
     }
   , { file: "graphics/line-chart.fld"
     }
   , { file: "graphics/stacked-bar-chart.fld"
     }
   ]
