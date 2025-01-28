module Module.Web
   ( loadFile
   , loadFile'
   , parseProgram
   , module_
   , datasetAs
   , loadProgCxt
   , module F
   , module Module
   , prepConfig
   ) where

import Prelude

import Affjax (Error(..)) as A
import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bind (Bind)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (class MonadError, ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.HTTP.Method (Method(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import Lattice (Raw)
import Module (Config, initialConfig, parse, prependFolder')
import Module (FileLoader, Folder(..), File(..)) as F
import Module (datasetAs, loadProgCxt, module_, parseProgram, prepConfig) as M
import ProgCxt (ProgCxt)
import SExpr (Expr) as S
import Util (type (×), AffError, debug, (×))

loadFile :: forall m. F.FileLoader m
loadFile folders file = do
   result <- runExceptT do
      let urls = map (\folder -> prependFolder' folder file) folders
      (_ × (F.File url)) <- ExceptT $ liftAff $ findM' urls
         ( \(F.File url) ->
              request (defaultRequest { url = url, method = Left HEAD, responseFormat = string })
         )
      when debug.logging $ liftAff $ log ("loadFile: resolved URL: " <> url)
      contents <- ExceptT $ liftAff $ request (defaultRequest { url = url, method = Left GET, responseFormat = string })
      pure contents.body
   case result of
      Left err -> throwError $ E.error $ printError err
      Right body -> pure body

findM' :: forall m f a b. Foldable f => f a -> (a -> AffError m (Either A.Error b)) -> AffError m (Either A.Error (b × a))
findM' collection func = foldr
   ( \a b -> do
        result <- func a
        case result of
           Left _ -> b
           Right found -> pure (Right (found × a))
   )
   (pure (Left (A.RequestFailedError)))
   collection

loadFile' :: forall m. Array F.Folder -> F.File -> AffError m (F.File × String)
loadFile' folders file = (file × _) <$> loadFile folders file

parseProgram :: forall m. Array F.Folder -> F.File -> AffError m (Raw S.Expr)
parseProgram = M.parseProgram loadFile

module_ :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> F.File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ = M.module_ loadFile

datasetAs :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> Bind F.File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs = M.datasetAs loadFile

loadProgCxt :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt fluidSrcPaths = M.loadProgCxt { loadFile, fluidSrcPaths }

prepConfig :: forall m. MonadAff m => MonadError Error m => Array F.Folder -> F.File -> ProgCxt Unit -> m Config
prepConfig fluidSrcPaths = M.prepConfig { loadFile, fluidSrcPaths }
