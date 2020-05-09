module Test.Parse where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (runParser)
import Parse (expr)
import Pretty (pretty)

testParse1 :: Effect Unit
testParse1 = do
   text <- readTextFile ASCII "../fluid/example/parser-wip.fld"
   case runParser text expr of
      Left error -> log $ show error
      Right e -> log $ show $ pretty e
