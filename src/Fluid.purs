module Fluid where

import Prelude hiding (between)

import Bind (Bind, (↦))
import Data.Array (filter, fromFoldable)
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripPrefix, stripSuffix, trim)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import EvalGraph (graphEval)
import Lattice (erase)
import Module.Node (File(..), Folder(..), loadProgCxt, prepConfig)
import Node.Buffer (toString)
import Node.ChildProcess (ChildProcess, ExecOptions, exec)
import Node.Encoding (Encoding(..))
import Options.Applicative (Parser, command, eitherReader, execParser, fullDesc, header, help, helper, long, many, option, progDesc, short, strOption, subparser, value, (<**>))
import Options.Applicative.Builder (info)
import Pretty (prettyP)
import Util (Endo)
import Val (Val)

data Program = Program
   { imports :: Array String
   , datasets :: Array (Bind String)
   , fileName :: String
   }

data Command = Evaluate Program | Publish Program Folder

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

publish :: Parser Command
publish = Publish <$> program <*> (Folder <$> strOption (long "website" <> short 'w' <> help "root directory of website under dist/" <> value "Misc"))

commandParser :: Parser Command
commandParser = subparser
   ( command "evaluate" (info (Evaluate <$> program) (progDesc "Evaluate a file"))
        <> command "publish" (info publish (progDesc "Publish a file"))
   )

dispatchCommand ∷ Command → Aff (Val Unit)
dispatchCommand = case _ of
   Evaluate p -> output p
   Publish p (Folder website) -> do
      _ <- liftEffect $ copyFiles website
      log "Published"
      output p

copyOptions :: ExecOptions
copyOptions = { cwd: Nothing, env: Nothing, timeout: Nothing, killSignal: Nothing, maxBuffer: Nothing, uid: Nothing, gid: Nothing, encoding: Nothing, shell: Nothing }

copyFiles ∷ String -> Effect ChildProcess
copyFiles website = do
   let root = "node_modules/@explorable-viz/fluid/"
   exec ("./" <> root <> "script/bundle-website.sh -w " <> website <> " -r true") copyOptions \{ error, stdout } -> do
      case error of
         (Just err) -> logShow err
         Nothing -> do
            out <- toString ASCII stdout
            log out

main :: Effect Unit
main = runAff_ callback do
   dispatchCommand =<< (liftEffect $ execParser opts)
   where
   opts = info (commandParser <**> helper) (fullDesc <> progDesc "Parse a file" <> header "parse - a simple parser")

callback :: Either Error (Val Unit) -> Effect Unit
callback = case _ of
   Left err -> logShow err
   Right v -> log (prettyP v)

output :: Program -> Aff (Val Unit)
output (Program { imports, datasets, fileName }) = do
   progCxt <- loadProgCxt imports datasets
   { e, gconfig } <- prepConfig (File fileName) progCxt
   { outα } <- graphEval gconfig e
   pure (erase outα)

