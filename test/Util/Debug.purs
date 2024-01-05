module Test.Util.Debug where

-- These flags considered only when Util.debug.tracing is true.
tracing
   :: { graphCreation :: Boolean
      , graphInputSize :: Boolean
      , graphBwdSliceInput :: Boolean
      , graphBwdSliceOutput :: Boolean
      , graphFwdSliceInput :: Boolean
      , graphFwdSliceOutput :: Boolean
      , checkEqual :: Boolean
      }

tracing =
   { graphCreation: false
   , graphInputSize: false
   , graphBwdSliceInput: false
   , graphBwdSliceOutput: false
   , graphFwdSliceInput: false
   , graphFwdSliceOutput: false
   , checkEqual: false
   }

-- Invariants that are potentially expensive to check and that we might want to disable in production,
-- that are not covered explicitly by tests.
checking
   :: { edgeListIso :: Boolean
      , inputsInGraph :: Boolean
      , outputsInGraph :: Boolean
      , allocRoundTrip :: Boolean
      }

checking =
   { edgeListIso: false
   , inputsInGraph: true
   , outputsInGraph: true
   , allocRoundTrip: true
   }

-- Should be set to true except when there are specific outstanding problems.
testing
   :: { fwdPreservesTop :: Boolean
      , bwdDuals :: Boolean
      , fwdDuals :: Boolean
      , naiveFwd :: Boolean
      }

testing =
   { fwdPreservesTop: true
   , bwdDuals: false
   , fwdDuals: true
   , naiveFwd: true
   }