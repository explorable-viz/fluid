module Test.Parse where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Parse (expr)
import Pretty (pretty)


testParse1 :: Effect Unit
testParse1 = do
   text <- readTextFile ASCII "../fluid/example/parser-wip.fld"
   let result = runParser text expr
   launchAff_ $ runSpec [consoleReporter] do
      describe "Parse" do
         it "blah" do
            case result of
               Left error -> do
                  true `shouldEqual` false
               Right e -> do
                  (show $ pretty e) `shouldEqual` "x + 100"
