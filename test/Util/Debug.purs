module Test.Util.Debug where

debug
   :: { logging :: Boolean -- logging to console
      }

debug =
   { logging: false
   }

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
