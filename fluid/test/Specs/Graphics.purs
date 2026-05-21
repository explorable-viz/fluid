module Test.Specs.Graphics where

import Prelude

import Test.Util.Suite (TestSpec)

graphics_cases :: Array TestSpec
graphics_cases =
   [ { file: "graphics/background.fld"
     , fwd_expect: mempty
     }
   , { file: "graphics/grouped_bar_chart.fld"
     , fwd_expect: mempty
     }
   , { file: "graphics/line_chart.fld"
     , fwd_expect: mempty
     }
   , { file: "graphics/stacked_bar_chart.fld"
     , fwd_expect: mempty
     }
   ]
