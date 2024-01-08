module Test.Util.Debug where

-- These flags considered only when Util.debug.tracing is true.
tracing
   :: { runWithGraphT :: Boolean
      , graphBwdSlice :: Boolean
      , graphFwdSlice :: Boolean
      , checkEqual :: Boolean
      , bwdSelection :: Boolean
      , fwdAfterBwd :: Boolean
      }

tracing =
   { runWithGraphT: true
   , graphBwdSlice: false
   , graphFwdSlice: true
   , checkEqual: false
   , bwdSelection: false
   , fwdAfterBwd: false
   }

-- Invariants that are potentially expensive to check and that we might want to disable in production,
-- that are not covered explicitly by tests.
checking
   :: { edgeListGC :: Boolean
      , edgeListSorted :: Boolean
      , inputsInGraph :: Boolean
      , outputsInGraph :: Boolean
      , allocRoundTrip :: Boolean
      }

checking =
   { edgeListGC: true
   , edgeListSorted: false
   , inputsInGraph: true
   , outputsInGraph: true
   , allocRoundTrip: false
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