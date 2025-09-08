module Temp.Util.Suite2 where

import Prelude

import Data.Array (drop, elem, filter, null, replicate)
import Data.Either (Either(..))
import Data.String (Pattern(..), joinWith)
import Data.String.CodeUnits (takeRight)
import Data.String.Common (replace)
import Data.String.Pattern (Replacement(..))
import Data.Traversable (foldl, traverse, traverse_)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, mkdir, readTextFile, readdir, rm, writeTextFile)
import Node.Process (argv)
import Parse as P
import Parsing (runParser)
import Pretty (class Pretty, prettyP)
import SExpr (Expr)
import Temp.Parse (parsePy)
import Temp.Pretty (prettyPy)
import Temp.Util.Error (prettyParseError)
import Util ((×))

parse :: String -> Either String (Expr Unit)
parse src = case runParser src P.program of
   Left e -> Left $ prettyParseError e
   Right (expr × _) -> Right expr

pretty :: forall d. Pretty d => d -> String
pretty = prettyP

srcDir :: String
srcDir = "syntax-migration-tests/suite"

reportDir :: String
reportDir = "syntax-migration-tests/reports"

srcFilename :: String -> String
srcFilename report = replace (Pattern ".md") (Replacement ".fld") report

reportFilename :: String -> String
reportFilename src = replace (Pattern ".fld") (Replacement ".md") src

cleanReports :: Array String -> Effect Unit
cleanReports srcFiles = do
   whenM (not <$> exists reportDir) (mkdir reportDir)
   reports <- readdir reportDir
   let staleReports = filter stale reports
   traverse_ (\f -> rm (reportDir <> "/" <> f)) staleReports
   where
   stale report = not $ elem (srcFilename report) srcFiles

main :: Effect Unit
main = do

   args <- argv
   let include = drop 2 args

   allFiles <- readdir srcDir
   let fluidFiles = filter (\f -> takeRight 4 f == ".fld") allFiles
   let filesToTest = if null include then fluidFiles else filter (\f -> elem f include) fluidFiles

   cleanReports filesToTest

   results <- traverse test filesToTest

   let { passes, fails, missing } = tally results
   let
      colour =
         if fails == 0 && missing == 0 then green
         else if fails == 0 then yellow
         else red

   log $ colour $ "\n" <> show fails <> " fail, " <> show missing <> " missing, " <> show passes <> " pass "

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

test :: String -> Effect Result
test srcFile = do
   src <- readTextFile UTF8 (srcDir <> "/" <> srcFile)
   case parse src of
      Left err ->
         fail
            "FIRST PARSE"
            $ block "src" src
                 <> block "error" err
      Right _expr ->
         let
            _srcPretty = pretty _expr
         in
            case parse _srcPretty of
               Left err ->
                  fail "SECOND PARSE" $
                     block "src" src
                        <> block "pretty" _srcPretty
                        <> block "error" err
               Right expr ->
                  let
                     srcPretty = pretty expr
                     srcPrettyPy = prettyPy expr

                  in
                     case (parsePy srcPrettyPy) of
                        Left err -> fail "PYTHONIC PARSE" $
                           block "pretty" srcPretty
                              <> block "pythonic" srcPrettyPy
                              <> block "error" err
                        Right pythonicParsed ->
                           let
                              resPretty = prettyP pythonicParsed
                           in
                              if resPretty == srcPretty then do
                                 whenM (exists reportPath) (rm reportPath)
                                 pure Pass
                              else do
                                 fail "MISMATCH"
                                    $ block "pretty" srcPretty
                                         <> block "result" resPretty
                                         <> block "pythonic" srcPrettyPy
                                         <> block "pretty ast" (show expr)
                                         <> block "pythonic ast" (show pythonicParsed)

   where

   reportPath :: String
   reportPath = reportDir <> "/" <> reportFilename srcFile

   fail :: String -> String -> Effect Result
   fail reason report = do
      writeTextFile UTF8 reportPath (h1 srcFile <> p reason <> report)
      log $ red ("✘ " <> reportPath <> " [" <> reason <> "]")
      pure Fail

red :: String -> String
red text = "\x1b[31m" <> text <> reset

green :: String -> String
green text = "\x1b[32m" <> text <> reset

yellow :: String -> String
yellow text = "\x1b[33m" <> text <> reset

block :: String -> String -> String
block t c = h2 t <> code c

h1 :: String -> String
h1 = h 1

h2 :: String -> String
h2 = h 2

h :: Int -> String -> String
h n c = (joinWith "" (replicate n "#")) <> " " <> c <> "\n\n"

p :: String -> String
p c = c <> "\n\n"

code :: String -> String
code c = "```\n" <> c <> "\n" <> "```\n\n"

reset :: String
reset = "\x1b[0m"
