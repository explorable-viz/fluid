module Test.Util.Microtime
   ( microtime
   ) where

-- import Prelude

import Effect (Effect)

foreign import microtime :: Effect Number