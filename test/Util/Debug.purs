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
      }

tracing =
   { graphCreation: false
   }

checking
   :: { bwdDuals :: Boolean
      , edgeListIso :: Boolean
      }

checking =
   { bwdDuals: false
   , edgeListIso: false
   }
