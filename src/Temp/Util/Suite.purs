module Temp.Util.Suite where

import Prelude

import Data.Array (drop, elem, filter, index, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (takeRight)
import Data.String.Common (replace)
import Data.String.Pattern (Replacement(..))
import Data.Traversable (foldl, traverse)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw, try)
import Module (parse)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, readdir, writeTextFile)
import Node.Process (argv)
import Parse as P
import Pretty (prettyP)
import Temp.Parse (parsePy)
import Temp.Pretty (prettyPy)

type TestFn = String -> Effect (Either String String)

parseArgs :: Effect {fn :: TestFn, dir :: String, files :: Array String}
parseArgs = do
   args <- argv
   case (index args 2) of
      Nothing -> throw "missing"
      Just "pretty" -> pure $ {fn: testPretty, dir: "test/golden/pretty", files: drop 3 args}
      Just "parse" -> pure $ {fn: testParse, dir: "test/golden/parse", files: drop 3 args}
      Just _ -> throw "unsupported"

main :: Effect Unit
main = do
   {fn, dir, files} <- parseArgs
   files' <- filter (\s -> takeRight 4 s == ".fld") <$> readdir dir
   let files'' = if null files then files' else filter (\s -> elem s files) files'
   results <- traverse (test fn dir) files''
   let { passes, fails, missing } = tally results
   let
      colour =
         if fails == 0 && missing == 0 then green
         else if fails == 0 then yellow
         else red

   log "\n"
   log $ colour $ show fails <> " fail, " <> show missing <> " missing, " <> show passes <> " pass "

   pure unit

data Result = Pass | Fail | Missing | Error String
type Counts = { passes :: Int, fails :: Int, missing :: Int }

tally :: Array Result -> Counts
tally = foldl tally' { passes: 0, fails: 0, missing: 0 }
   where
   tally' acc Pass = acc { passes = acc.passes + 1 }
   tally' acc Fail = acc { fails = acc.fails + 1 }
   tally' acc Missing = acc { missing = acc.missing + 1 }
   tally' acc _ = acc -- ignore Error cases




testPretty :: TestFn
testPretty src = do
   parsed <- try $ snd <$> parse src P.program
   case parsed of
      Left error -> pure $ Left (show error)
      Right expr -> pure $ Right (prettyPy expr <> "\n")

testParse :: TestFn
testParse src = do
   parsed <- try $ parsePy src
   case parsed of
      Left error -> pure $ Left (show error)
      Right expr -> pure $ Right (prettyP expr <> "\n")

test :: TestFn -> String -> String -> Effect Result
test f srcDir srcFile = do
   let srcPath = srcDir <> "/" <> srcFile
   let expectPath = replace (Pattern ".fld") (Replacement ".expect") srcPath
   src <- readTextFile UTF8 srcPath
   result <- f src
   case result of
      Left error -> do
         log $ red ("✘ " <> srcFile)
         log $ show error
         pure Fail
      Right out -> do
         hasExpect <- exists expectPath
         if hasExpect then do
            expect <- readTextFile UTF8 expectPath
            if out == expect then
               pure Pass
            else do
               log $ red ("✘ " <> srcFile)
               log $ out
               pure Fail
         else do
            writeTextFile UTF8 expectPath out
            log $ yellow ("~ " <> srcFile)
            pure Missing

red :: String -> String
red text = "\x1b[31m" <> text <> reset

green :: String -> String
green text = "\x1b[32m" <> text <> reset

yellow :: String -> String
yellow text = "\x1b[33m" <> text <> reset

reset :: String
reset = "\x1b[0m"
