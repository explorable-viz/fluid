module Module.Web
   ( loadFile
   , loadFile'
   , parseProgram
   , open
   , module_
   , datasetAs
   , loadProgCxt
   , module F
   , module Module
   , prepConfig
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
import Lattice (Raw)
import Module (Config, initialConfig, parse)
import Module (FileLoader, Folder(..), File(..)) as F
import Module (datasetAs, loadProgCxt, module_, parseProgram, prepConfig) as M
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (type (×), AffError, (×))

loadFile :: forall m. F.FileLoader m
loadFile (F.Folder folder) (F.File file) = do
   let url = folder <> "/" <> file <> ".fld"
   result <- liftAff $ request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> do
         log ("Failed with " <> printError err)
         throwError $ E.error $ printError err
      Right response ->
         pure response.body

loadFile' :: forall m. F.Folder -> F.File -> AffError m (F.File × String)
loadFile' folder file = (file × _) <$> loadFile folder file

parseProgram :: forall m. F.Folder -> F.File -> AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

open :: forall m. F.File -> AffError m (Raw S.Expr)
open = parseProgram (F.Folder "fluid/example")

module_ :: forall m. MonadAff m => MonadError Error m => F.File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => Bind F.File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

loadProgCxt :: forall m. MonadAff m => MonadError Error m => F.Folder -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt fluidSrcPath = M.loadProgCxt { loadFile, fluidSrcPath }

prepConfig :: forall m. MonadAff m => MonadError Error m => F.Folder -> F.File -> ProgCxt Unit -> m Config
prepConfig fluidSrcPath = M.prepConfig { loadFile, fluidSrcPath }
