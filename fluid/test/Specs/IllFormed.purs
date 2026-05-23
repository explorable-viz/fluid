module Test.Specs.IllFormed where

import Test.Util.Suite (IllFormedSpec)

illFormed_cases :: Array IllFormedSpec
illFormed_cases =
   [ { file: "unreachable.fld", expected_error: "Unreachable statement" }
   ]
