module Fluid where

import Prelude hiding (between)

import Bind (Bind, (↦))
import Data.Array (filter, fromFoldable)
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripPrefix, stripSuffix, trim)
import Data.String as String
import Desugarable (desug)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import EvalGraph (graphEval)
import Lattice (erase)
import Module.Files (File(..))
import Module.Node (initialConfig, loadProgCxt, open)
import Options.Applicative (Parser, eitherReader, execParser, fullDesc, header, help, helper, long, many, option, progDesc, short, strOption, (<**>))
import Options.Applicative.Builder (info)
import Pretty (prettyP)
import Util (Endo)
import Val (Val)

data Program = Program
   { imports :: Array String
   , datasets :: Array (Bind String)
   , fileName :: String
   }

between :: forall a. Pattern -> Pattern -> Endo (String -> Either String a)
between p1 p2 = \f s -> do
   case (stripPrefix p1) s >>= (stripSuffix p2) of
      Just rest -> f rest
      Nothing -> Left ("Expected " <> show p1 <> "..." <> show p2 <> " but got ...")

parsePair :: String -> Either String (Bind String)
parsePair = between (Pattern "(") (Pattern ")") $ \s -> do
   case split (Pattern ",") s of
      [ k, v ] -> Right (trim k ↦ trim v)
      _ -> Left $ "Expected a pair but got " <> s

parseDataset' :: String -> Either String (Bind String)
parseDataset' = \s -> do
   dataset <- parsePair s
   Right (dataset :: (Bind String))

parseImports' :: Pattern -> Pattern -> (String -> Either String (Array String))
parseImports' open close = between open close $ \s -> do
   Right (map trim $ filter (not <<< String.null) $ split (Pattern ",") s)

parseDatasets :: Parser (List (Bind String))
parseDatasets =
   many $ option (eitherReader $ parseDataset')
      ( long "datasets"
           <> short 'd'
           <> help "A comma separated list of datasets"
      )

parseImports :: Parser (List String)
parseImports =
   many $ strOption
      ( long "imports"
           <> short 'i'
           <> help "A comma separated list of import file locations"
      )

program :: Parser Program
program = ado
   imports <- fromFoldable <$> parseImports
   datasets <- fromFoldable <$> parseDatasets
   fileName <- strOption (long "file" <> short 'f' <> help "The file to parse")
   in Program { imports, datasets, fileName }

main :: Effect Unit
main = runAff_ callback do
   output =<< (liftEffect $ execParser opts)
   where
   opts = info (program <**> helper) (fullDesc <> progDesc "Parse a file" <> header "parse - a simple parser")

callback :: Either Error (Val Unit) -> Effect Unit
callback = case _ of
   Left err -> logShow err
   Right v -> log (prettyP v)

output :: Program -> Aff (Val Unit)
output (Program { imports, datasets, fileName }) = do
   progCxt <- loadProgCxt imports datasets
   s <- open (File fileName)
   e <- desug s
   gconfig <- initialConfig e progCxt
   { outα } <- graphEval gconfig e
   pure (erase outα)

