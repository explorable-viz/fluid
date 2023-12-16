module Test.Util.Debug where

debug
   :: { logging :: Boolean
      , checkBwdDuals :: Boolean
      }
debug =
   { logging: true
   , checkBwdDuals: false
   }
