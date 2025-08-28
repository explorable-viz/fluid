module Temp.Pretty.Config where

config
   :: { indentation :: Int
      , inlineRecordLimit :: Int
      , inlineBlockLimit :: Int
      }
config =
   { indentation: 2
   , inlineRecordLimit: 50
   , inlineBlockLimit: 20
   }
