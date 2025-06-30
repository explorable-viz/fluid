module Module.Web
   ( loadFile
   , loadFile'
   , parseProgram
   , module_
   , datasetAs
   , loadProgCxt
   , module Module
   , prepConfig
   ) where

import Prelude

import Affjax (Error(..)) as A
import Affjax (Response)
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (defaultRequest, printError, request)
import Bind (Bind)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (class MonadError, ExceptT(..), runExceptT)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import File (FileLoader, Folder, File(..), prependFolder)
import Lattice (Raw)
import Module (Config, initialConfig, parse)
import Module (datasetAs, loadProgCxt, module_, parseProgram, prepConfig) as M
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (type (×), (×), AffError, debug, findM)

loadFile :: forall m. FileLoader m
loadFile folders (File file) = do
   let urls = flip prependFolder (File $ file <> ".fld") <$> folders
   result <- runExceptT $ do
      (_ × (url')) <- ExceptT $ liftAff $ findM urls checkUrl (Left A.RequestFailedError)
      when debug.logging $ liftAff $ log ("loadFile: resolved URL: " <> url')
      contents <- ExceptT $ liftAff $ request (defaultRequest { url = url', method = Left GET, responseFormat = string })
      pure contents.body
   either (throwError <<< E.error <<< printError) pure result
   where
   checkUrl :: File -> Aff (Either A.Error (Response String × String))
   checkUrl (File url) = do
      resp <- request (defaultRequest { url = url, method = Left HEAD, responseFormat = string })
      pure case resp of
         Right resp' | resp'.status == StatusCode 200 -> Right (resp' × url)
         Right _ -> Left A.RequestFailedError
         Left err -> Left err

loadFile' :: forall m. Array Folder -> File -> AffError m (File × String)
loadFile' folders file = (file × _) <$> loadFile folders file

parseProgram :: forall m. Array Folder -> File -> AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

module_ :: forall m. MonadAff m => MonadError Error m => Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array Folder -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt fluidSrcPaths = M.loadProgCxt { loadFile, fluidSrcPaths }

prepConfig :: forall m. MonadAff m => MonadError Error m => Array Folder -> File -> ProgCxt Unit -> m Config
prepConfig fluidSrcPaths = M.prepConfig { loadFile, fluidSrcPaths }
