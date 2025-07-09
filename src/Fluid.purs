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
import File (File(..), Folder(..))
import Lattice (erase)
import Module (loadProgCxt, prepConfig)
import Module.Node (runNodeT)
import Node.Buffer (toString)
import Node.ChildProcess (ChildProcess, ExecOptions, exec)
import Node.Encoding (Encoding(..))
import Options.Applicative (Parser, command, eitherReader, execParser, fullDesc, header, help, helper, long, many, option, progDesc, short, strOption, subparser, switch, value, (<**>))
import Options.Applicative.Builder (info)
import Pretty (prettyP)
import Util (Endo)
import Val (Val)

data EvalArgs = EvalArgs
   { local :: Boolean
   , imports :: Array String
   , datasets :: Array (Bind String)
   , fileName :: String
   , fluidSrcPath :: Folder
   }

data BundleArgs = BundleArgs Folder Boolean

data Command = Evaluate EvalArgs | BundleWebsite BundleArgs

between :: forall a. Pattern -> Pattern -> Endo (String -> Either String a)
between p1 p2 f s =
   case (stripPrefix p1) s >>= stripSuffix p2 of
      Just rest -> f rest
      Nothing -> Left ("Expected " <> show p1 <> "..." <> show p2 <> " but got ...")

parsePair :: String -> Either String (Bind String)
parsePair = between (Pattern "(") (Pattern ")") $ \s ->
   case split (Pattern ",") s of
      [ k, v ] -> Right (trim k ↦ trim v)
      _ -> Left $ "Expected a pair but got " <> s

parseImports' :: Pattern -> Pattern -> (String -> Either String (Array String))
parseImports' open close = between open close $ \s -> do
   Right (map trim $ filter (not <<< String.null) $ split (Pattern ",") s)

parseDatasets :: Parser (List (Bind String))
parseDatasets =
   many $ option (eitherReader parsePair)
      ( long "datasets"
           <> short 'd'
           <> help "Comma-separated list of datasets"
      )

parseImports :: Parser (List String)
parseImports =
   many $ strOption
      ( long "imports"
           <> short 'i'
           <> help "Comma-separated list of files to import"
      )

parseLocal :: Parser Boolean
parseLocal = switch (long "local" <> short 'l' <> help "Are you running fluid as a library?")

parseEvaluate :: Parser EvalArgs
parseEvaluate = ado
   local <- parseLocal
   imports <- fromFoldable <$> parseImports
   datasets <- fromFoldable <$> parseDatasets
   fileName <- strOption (long "file" <> short 'f' <> help "The file to parse")
   fluidSrcPath <- Folder <$> strOption (long "fluid-src-path" <> short 'p' <> help "The path containing the program files")
   in EvalArgs { local, imports, datasets, fileName, fluidSrcPath }

parseBundleArgs :: Parser BundleArgs
parseBundleArgs = ado
   website <- Folder <$> strOption (long "website" <> short 'w' <> help "root directory of website under dist/" <> value "Misc")
   local <- parseLocal
   in BundleArgs website local

commands :: { bundleWebsite :: Parser Command, evaluate :: Parser Command }
commands =
   { bundleWebsite: BundleWebsite <$> parseBundleArgs
   , evaluate: Evaluate <$> parseEvaluate
   }

commandParser :: Parser Command
commandParser = subparser
   ( command "evaluate" (info commands.evaluate (progDesc "Evaluate a file"))
        <> command "bundle-website" (info commands.bundleWebsite (progDesc "Bundle a website to dist"))
   )

dispatchCommand ∷ Command → Aff Unit
dispatchCommand (Evaluate p) = do
   v <- evaluate p
   log (prettyP v)
dispatchCommand (BundleWebsite bas) =
   void $ liftEffect $ bundleWebsite bas

copyOptions :: ExecOptions
copyOptions =
   { cwd: Nothing
   , env: Nothing
   , timeout: Nothing
   , killSignal: Nothing
   , maxBuffer: Nothing
   , uid: Nothing
   , gid: Nothing
   , encoding: Nothing
   , shell: Nothing
   }

bundleWebsite ∷ BundleArgs -> Effect ChildProcess
bundleWebsite (BundleArgs (Folder website) local) =
   exec cmd copyOptions \{ error, stdout } ->
      case error of
         Just err -> logShow err
         Nothing -> log =<< toString ASCII stdout
   where
   cmd = "."
      <> (if local then "/" <> fluidLibraryPath else "")
      <> "/script/bundle-website.sh -w "
      <> website
      <> if local then " -l" else ""

main :: Effect Unit
main = runAff_ callback (dispatchCommand =<< liftEffect (execParser opts))
   where
   opts = info (commandParser <**> helper) (fullDesc <> progDesc "Parse a file" <> header "parse - a simple parser")

callback :: Either Error Unit -> Effect Unit
callback = case _ of
   Left err -> logShow err
   Right _ -> pure unit

fluidLibraryPath :: String
fluidLibraryPath = "node_modules/@explorable-viz/fluid"

evaluate :: EvalArgs -> Aff (Val Unit)
evaluate (EvalArgs { local, imports, datasets, fileName, fluidSrcPath }) = do
   let fluidSrcPaths = [ fluidSrcPath ] <> if local then [ Folder (fluidLibraryPath <> "/dist/fluid/fluid") ] else []
   runNodeT $ do
      progCxt <- loadProgCxt { fluidSrcPaths } imports datasets
      { e, gconfig } <- prepConfig { fluidSrcPaths } (File fileName) progCxt
      { outα } <- graphEval gconfig e
      pure (erase outα)
