module Test.Util.Debug where

-- These flags considered only when Util.debug.tracing is true.
tracing
   :: { runWithGraphT :: Boolean
      , graphBwdSlice :: Boolean
      , graphFwdSlice :: Boolean
      , checkEq :: Boolean
      , bwdSelection :: Boolean
      , fwdAfterBwd :: Boolean
      , mediatingData :: Boolean
      , mouseEvent :: Boolean
      }

tracing =
   { runWithGraphT: false
   , graphBwdSlice: false
   , graphFwdSlice: false
   , checkEq: false
   , bwdSelection: false
   , fwdAfterBwd: false
   , mediatingData: false
   , mouseEvent: false
   }

-- Invariants that are potentially expensive to check and that we might want to disable in production,
-- that are not covered explicitly by tests.
checking
   :: { edgeListGC :: Boolean
      , edgeListSorted :: Boolean
      , inputsAreSinks :: Boolean
      , outputsInGraph :: Boolean
      , allocRoundTrip :: Boolean
      }

checking =
   { edgeListGC: true
   , edgeListSorted: true
   , inputsAreSinks: true
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
   , bwdDuals: true
   , fwdDuals: true
   , naiveFwd: true
   }

timing
   :: { selectionResult :: Boolean
      }

timing =
   { selectionResult: false
   }
