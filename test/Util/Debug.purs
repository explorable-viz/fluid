module Test.Util.Debug where

debug
   :: { logging :: Boolean -- logging via "log"; requires an effect context
      }

debug =
   { logging: false
   }

-- tracing via "trace"; no effect context required
tracing
   :: { graphCreation :: Boolean
      , graphBwdSliceInput :: Boolean
      , graphBwdSliceOutput :: Boolean
      , graphFwdSliceInput :: Boolean
      , graphFwdSliceOutput :: Boolean
      }

tracing =
   { graphCreation: false
   , graphBwdSliceInput: false
   , graphBwdSliceOutput: false
   , graphFwdSliceInput: false
   , graphFwdSliceOutput: false
   }

-- Invariants that are potentially expensive to check and that we might want to disable in production,
-- that are not covered explicitly by tests.
asserting
   :: { edgeListIso :: Boolean
      , sinksAreInputs :: Boolean
      }

asserting =
   { edgeListIso: false
   , sinksAreInputs: false
   }

-- Ideally, should always be true, and only disabled when there are specific outstanding problems.
testing
   :: { bwdDuals :: Boolean
      , fwdDuals :: Boolean
      , naiveFwd :: Boolean
      }

testing =
   { bwdDuals: false
   , fwdDuals: false
   , naiveFwd: false
   }
