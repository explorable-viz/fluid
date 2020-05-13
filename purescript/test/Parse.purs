module Test.Parse where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Parse (program)
import Pretty (prettyProgram)


testParse1 :: Effect Unit
testParse1 = do
   text <- readTextFile ASCII "../fluid/example/parser-wip.fld"
   let result = runParser text program
   launchAff_ $ runSpec [consoleReporter] do
      describe "Parse" do
         it "blah" do
            case result of
               Left error -> do
                  log $ show error
                  true `shouldEqual` false
               Right e -> do
                  (show $ prettyProgram e) `shouldEqual` text
