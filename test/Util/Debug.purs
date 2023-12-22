module Test.Util.Debug where

debug
   :: { logging :: Boolean -- logging via "log"; requires an effect context
      }

debug =
   { logging: true
   }

-- tracing via "trace"; no effect context required
tracing
   :: { graphCreation :: Boolean
      }

tracing =
   { graphCreation: false
   }

-- TODO: partition into properties we want to test all the time vs. debug-only properties
checking
   :: { bwdDuals :: Boolean
      , edgeListIso :: Boolean
      , sinksAreInputs :: Boolean
      }

checking =
   { bwdDuals: false
   , edgeListIso: false
   , sinksAreInputs: true
   }
