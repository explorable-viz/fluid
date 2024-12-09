module Fluid where

import Prelude hiding (between)

import Bind (Bind, (↦))
import Control.Alt ((<|>))
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
import Module (Folder(..), File(..))
import Module.Node (initialConfig, loadProgCxt, open)
import Options.Applicative (Parser, eitherReader, execParser, fullDesc, header, help, helper, long, option, progDesc, short, strOption, (<**>))
import Options.Applicative.Builder (info)
import Pretty (prettyP)
import Util (Endo)
import Val (Val)

data Program = Program
   { imports :: Array String
   , datasets :: Array (Bind String)
   , root :: String
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

parseDatasets' :: String -> Either String (Array (Bind String))
parseDatasets' = \s -> do
   let pairs = map trim $ split (Pattern " ") s
   datasets <- traverse parsePair pairs
   Right (datasets :: Array (Bind String))

parseImports' :: Pattern -> Pattern -> (String -> Either String (Array String))
parseImports' open close = between open close $ \s -> do
   Right (map trim $ filter (not <<< String.null) $ split (Pattern ",") s)

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

parseRoot :: Parser String
parseRoot =
   strOption (long "root" <> short 'r' <> help "The root director") <|> pure "../fluid/fluid"

program :: Parser Program
program = ado
   imports <- parseImports
   datasets <- parseDatasets
   root <- parseRoot
   fileName <- strOption (long "file" <> short 'f' <> help "The file to parse")
   in Program { imports, datasets, root, fileName }

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
output (Program { root, imports, datasets, fileName }) = do
   s <- open (Folder root) (File fileName)
   { e } :: Desugaring Unit <- desugGC s
   progCxt <- (loadProgCxt (Folder root) imports datasets)
   gconfig <- initialConfig e progCxt
   { outα } <- graphEval gconfig e
   pure (erase outα)

