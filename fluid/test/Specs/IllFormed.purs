module Test.Specs.IllFormed where

import Test.Util.Suite (IllFormedSpec)

ill_formed_cases :: Array IllFormedSpec
ill_formed_cases =
   [ { file: "unreachable.fld", expected_error: "Unreachable code after return" }
   ]
