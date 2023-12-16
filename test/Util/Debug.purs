module Test.Util.Debug where

debug
   :: { logging :: Boolean
      , check ::
           { bwdDuals :: Boolean
           , edgeListIso :: Boolean
           }
      }
debug =
   { logging: true
   , check:
        { bwdDuals: false
        , edgeListIso: false
        }
   }
