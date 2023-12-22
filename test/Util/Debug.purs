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

-- TODO: partition into properties we want to test all the time vs. debug-only properties
checking
   :: { bwdDuals :: Boolean
      , fwdDuals :: Boolean
      , edgeListIso :: Boolean
      , sinksAreInputs :: Boolean
      }

checking =
   { bwdDuals: false
   , fwdDuals: true
   , edgeListIso: false
   , sinksAreInputs: false
   }
