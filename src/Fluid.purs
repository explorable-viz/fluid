module Fluid where

import Prelude hiding (between)

import Bind (Bind, (↦))
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripPrefix, stripSuffix, trim)
import Data.String as String
import Data.Traversable (traverse)
import Desug (Desugaring, desugGC)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import EvalGraph (graphEval)
import Lattice (erase)
import Module.Local (File(..), initialConfig, loadProgCxt, open)
import Options.Applicative (Parser, eitherReader, execParser, fullDesc, header, help, helper, long, option, progDesc, short, strOption, (<**>))
import Options.Applicative.Builder (info)
import Pretty (prettyP)
import Val (Val)
import Control.Alt ((<|>))

-- import Util (error)

-- import Pretty (prettyP)

data Program = Program
   { imports :: Array String
   , datasets :: Array (Bind String)
   , fileName :: String
   }

instance Show Program where
   show (Program { fileName }) = "Program { fileName: " <> fileName <> " }"

parsePair :: String -> Either String (Bind String)
parsePair = \s -> do
   case stripPrefix (Pattern "(") s >>= stripSuffix (Pattern ")") of
      Just rest -> do
         case split (Pattern ",") rest of
            [ k, v ] -> Right (trim k ↦ trim v)
            _ -> Left $ "Expected a pair but got " <> s
      Nothing -> Left $ "Expected ( ... ) but got " <> s

parseDatasets' :: String -> Either String (Array (Bind String))
parseDatasets' = \s -> do
   case stripPrefix (Pattern "[") s >>= stripSuffix (Pattern "]") of
      Just rest -> do
         let pairs = map trim $ split (Pattern ";") rest
         datasets <- traverse parsePair pairs
         Right (datasets :: Array (Bind String))
      Nothing -> Left $ "Expected [ ... ] but got " <> s

parseImports' :: Pattern -> Pattern -> (String -> Either String (Array String))
parseImports' open close = \s -> do
   case (stripPrefix open s) >>= (stripSuffix close) of
      Just rest -> Right (map trim $ filter (not <<< String.null) $ split (Pattern ",") rest)
      Nothing -> Left $ "Expected " <> show open <> " ... " <> show close <> " but got " <> s

parseDatasets :: Parser (Array (Bind String))
parseDatasets =
   option (eitherReader $ parseDatasets')
      ( long "datasets"
           <> short 'd'
           <> help "A comma separated list of datasets"
      ) <|> pure []

parseImports :: Parser (Array String)
parseImports =
   option (eitherReader $ parseImports' (Pattern "[") (Pattern "]"))
      ( long "imports"
           <> short 'i'
           <> help "A comma separated list of import file locations"
      ) <|> pure []

program :: Parser Program
program = ado
   imports <- parseImports
   datasets <- parseDatasets
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
   s <- open (File fileName)
   { e } :: Desugaring Unit <- desugGC s
   progCxt <- (loadProgCxt imports datasets)
   gconfig <- initialConfig e progCxt
   { outα } <- graphEval gconfig e
   pure (erase outα)

