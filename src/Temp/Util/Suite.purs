module Temp.Util.Suite where

import Prelude

import Data.Array (drop, elem, filter, null)
import Data.Either (Either(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (takeRight)
import Data.String.Common (replace)
import Data.String.Pattern (Replacement(..))
import Data.Traversable (foldl, traverse)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (try)
import Module (parse)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, readdir, writeTextFile)
import Node.Process (argv)
import Parse as P
import Temp.Pretty (prettyPy)

dir :: String
dir = "test/golden/pretty"

main :: Effect Unit
main = do

   args <- argv
   let specified = drop 2 args

   files <- filter (\s -> takeRight 4 s == ".fld") <$> readdir dir
   let files' = if null specified then files else filter (\s -> elem s specified) files

   results <- traverse (process true dir) files'

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

process :: Boolean -> String -> String -> Effect Result
process create srcDir srcFile = do
   let srcPath = srcDir <> "/" <> srcFile
   let expectPath = replace (Pattern ".fld") (Replacement ".expect") srcPath

   src <- readTextFile UTF8 srcPath
   parsed <- try $ snd <$> parse src P.program

   case parsed of
      Left error -> do
         log $ red ("✘ " <> srcFile)
         log $ show error

         pure Fail
      Right parsed' -> do

         let pretty = prettyPy parsed' <> "\n"

         hasExpect <- exists expectPath

         if hasExpect then do
            expect <- readTextFile UTF8 expectPath
            if pretty == expect then do
               -- log $ (green "✔ ") <> srcFile
               pure Pass
            else do
               log $ red ("✘ " <> srcFile)
               log $ pretty
               pure Fail
         else do
            when create do
               writeTextFile UTF8 expectPath pretty

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
