module Test.Parse where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile ASCII "./input.txt"
  log text
