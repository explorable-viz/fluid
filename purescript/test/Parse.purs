module Test.Parse where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

testParse1 :: Effect Unit
testParse1 = do
  text <- readTextFile ASCII "./input.txt"
  log text
