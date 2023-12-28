module Test.Util.Debug where

-- specific flags considered only when debug.tracing is true
tracing
   :: { graphCreation :: Boolean
      , graphBwdSliceInput :: Boolean
      , graphBwdSliceOutput :: Boolean
      , graphFwdSliceInput :: Boolean
      , graphFwdSliceOutput :: Boolean
      }

tracing =
   { graphCreation: false
   , graphBwdSliceInput: true
   , graphBwdSliceOutput: true
   , graphFwdSliceInput: true
   , graphFwdSliceOutput: true
   }

-- Invariants that are potentially expensive to check and that we might want to disable in production,
-- that are not covered explicitly by tests.
checking
   :: { edgeListIso :: Boolean
      , sinksAreInputs :: Boolean
      }

checking =
   { edgeListIso: false
   , sinksAreInputs: false
   }

-- Should always be true, except when there are specific outstanding problems.
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
