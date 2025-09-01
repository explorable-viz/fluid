module Test.Specs.Graphics where

import Prelude

import Test.Util.Suite (TestSpec)

graphics_cases :: Array TestSpec
graphics_cases =
   [ { file: "graphics/background.fld"
     , fwd_expect: mempty
     }
   , { file: "graphics/grouped-bar-chart.fld"
     , fwd_expect: mempty
     }
   , { file: "graphics/line-chart.fld"
     , fwd_expect: mempty
     }
   , { file: "graphics/stacked-bar-chart.fld"
     , fwd_expect: mempty
     }
   ]
