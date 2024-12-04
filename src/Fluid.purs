module Fluid where

import Prelude

import Data.Either (Either(..))
import Desug (Desugaring, desugGC)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import EvalGraph (graphEval)
import Module (File(..), initialConfig, loadProgCxt, open)
import Options.Applicative (Parser, execParser, header, help, helper, long, progDesc, fullDesc, short, strOption, (<**>))
import Options.Applicative.Builder (info)
import Pretty (prettyP)

data Program = Program
   { fileName :: String
   }

instance Show Program where
   show (Program { fileName }) = "Program { fileName: " <> fileName <> " }"

program :: Parser Program
program = ado
   fileName <- strOption (long "file" <> short 'f' <> help "The file to parse")
   in Program { fileName }

main :: Effect Unit
main = runAff_ callback do
   output =<< (liftEffect $ execParser opts)
   where
   opts = info (program <**> helper) (fullDesc <> progDesc "Parse a file" <> header "parse - a simple parser")

callback :: forall a. Either Error a -> Effect Unit
callback = case _ of
   Left _ -> logShow "error"
   Right _ -> logShow "a"

output :: Program -> Aff Unit
output (Program { fileName }) = do
   s <- open (File fileName)
   { e } :: Desugaring Unit <- desugGC s
   initcxt <- loadProgCxt [] []
   gconfig <- initialConfig e initcxt
   { outα } <- graphEval gconfig e
   logShow $ prettyP outα
