module Test.Util.Debug where

debug
   :: { logging :: Boolean
      }

debug =
   { logging: false
   }

checking
   :: { bwdDuals :: Boolean
      , edgeListIso :: Boolean
      }

checking =
   { bwdDuals: false
   , edgeListIso: false
   }
