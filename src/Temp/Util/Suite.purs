module Temp.Util.Suite where

import Prelude

import Data.Traversable (traverse_)
import Effect (Effect)
import Module (parse)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, writeTextFile)
import Parse as P
import Pretty (prettyP)
import Temp.Pretty (prettyPy)
import Util ((×))

inputDir :: String
inputDir = "./suite/input/"

resultDir :: String
resultDir = "./suite/result/"

process :: String -> String -> String -> Effect Unit
process inDir outDir file = do
   src <- readTextFile UTF8 (inDir <> file)
   _ × s <- parse src P.program
   let pretty = prettyP s
   let prettypy = prettyPy s
   let result = "## Original syntax\n\n```fld\n" <> pretty <> "\n```\n\n## Pythonic syntax\n\n```fld2\n" <> prettypy <> "\n```"
   writeTextFile UTF8 (outDir <> file <> ".md") result

main :: Effect Unit
main = do
   files <- readdir inputDir
   traverse_ (process inputDir resultDir) files
