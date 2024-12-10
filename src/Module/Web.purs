module Module.Web
   ( loadFile
   , loadFile'
   , parse
   , parseProgram
   , open
   , module_
   , datasetAs
   , loadProgCxt
   , initialConfig
   ) where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bind (Bind)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig)
import Expr (class FV)
import Lattice (Raw)
import Module.Files (FileLoader, Folder(..), File(..))
import Module (datasetAs, loadProgCxt, module_, parse, parseProgram, initialConfig) as M
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (type (×), AffError, (×))
import Util.Parse (SParser)

loadFile :: FileLoader
loadFile (Folder folder) (File file) = do
   let url = "/" <> folder <> "/" <> file <> ".fld"
   result <- liftAff $ request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> do
         log ("Failed with " <> printError err)
         throwError $ E.error $ printError err
      Right response ->
         pure response.body

loadFile' :: forall m. Folder -> File -> AffError m (File × String)
loadFile' folder file = (file × _) <$> loadFile folder file

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse = M.parse

parseProgram :: forall m. Folder -> File -> AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

open :: forall m. File -> AffError m (Raw S.Expr)
open = parseProgram (Folder "fluid/example")

module_ :: forall m. MonadAff m => MonadError Error m => File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile (Folder "fluid")

datasetAs :: forall m. MonadAff m => MonadError Error m => Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile (Folder "fluid")

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt = M.loadProgCxt loadFile (Folder "fluid")

initialConfig :: forall m a. MonadError Error m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig = M.initialConfig
