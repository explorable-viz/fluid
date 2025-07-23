module App.LoadFigure where

import Prelude hiding (absurd)

import Affjax.ResponseFormat (json)
import Affjax.Web (get, printError)
import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind (Bind)
--import Control.Monad.Error.Class (catchError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
--import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeArray, decodeBoolean, decodeString, decodeTuple)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, uncurry)
import Doc (DocOpt(..))
import Effect (Effect)
--import Effect.Aff (Aff, Fiber, launchAff, launchAff_, throwError)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
--import Effect.Console (log)
import File (File(..), FileCxt(..), Folder(..))
import Graph (DVertex'(..))
import Module.Web (loadFile', runWebT)
import Util (error, (×))
import Val (Val(..), asVal)

type JsonSpec =
   { fluidSrcPath :: Array String
   , datasets :: Array (Bind String)
   , imports :: Array String
   , file :: String
   , inputs :: Array String
   , query :: Boolean
   , linking :: Boolean
   }

type DecodeJsonSpec =
   { fluidSrcPath :: Json
   , datasets :: Json
   , imports :: Json
   , file :: Json
   , inputs :: Json
   , query :: Json
   , linking :: Json
   }

decodeDatabase :: Json -> Either JsonDecodeError (Tuple String String)
decodeDatabase = decodeTuple decodeString decodeString

decodeJsonSpec :: Json -> Either JsonDecodeError JsonSpec
decodeJsonSpec json = do
   obj <- decodeJson json :: Either JsonDecodeError DecodeJsonSpec
   fluidSrcPath <- decodeArray decodeString obj.fluidSrcPath
   datasets <- decodeArray decodeDatabase obj.datasets
   imports <- decodeArray decodeString obj.imports
   file <- decodeString obj.file
   inputs <- decodeArray decodeString obj.inputs
   query <- decodeBoolean obj.query
   linking <- decodeBoolean obj.linking
   pure { fluidSrcPath, datasets, imports, file, inputs, query, linking }

figSpecFromJson :: JsonSpec -> FigSpec
figSpecFromJson spec@{ datasets, file, imports, inputs, query, linking } =
   { fluidSrcPaths: Folder <$> spec.fluidSrcPath
   , datasets
   , imports
   , file: File file
   , inputs
   , query:
        if query then
           Just $ asVal >=> case _ of
              v@(Val α (Doc _) _) -> Just $ DVertex (α × v)
              _ -> Nothing
        else Nothing
   , linking
   }

loadFigureOLD :: String -> Effect Unit -- TO DELETE
loadFigureOLD fileName = runAffs_ (uncurry drawFig)
   [ do
        -- TODO: simplify
        result <- get json fileName
        case result of
           Left err -> error ("Json fetching failed with " <> printError err)
           Right response ->
              case decodeJson response.body of
                 Left err -> error ("JSON decoding failed with " <> show err)
                 Right spec -> do
                    let spec'@{ fluidSrcPaths } = figSpecFromJson spec
                    ("fig" × _) <$> runWebT (FileCxt { fluidSrcPaths }) (loadFig spec')
   ]

loadFigureFromJsonSpec :: JsonSpec -> Effect Unit
loadFigureFromJsonSpec spec = runAffs_ (uncurry drawFig)
   [ let
        figSpec@{ fluidSrcPaths } = figSpecFromJson spec
     in
        ("fig" × _) <$> runWebT (FileCxt { fluidSrcPaths }) (loadFig figSpec)
   ]

loadSpec :: String -> Aff JsonSpec -- TO DO: Make exportable
loadSpec filename = do
   result <- get json filename
   case result of
      Left err -> throwError (error ("Json fetching failed with " <> printError err))
      Right response ->
         case decodeJson response.body of
            Left err -> throwError (error ("JSON decoding failed with " <> show err))
            Right spec -> pure $ spec

loadFigureFromSpec :: Json -> Effect Unit
loadFigureFromSpec encodedJsonSpec = case decodeJsonSpec encodedJsonSpec of
   Left err -> throwError (error ("JSON decoding failed with " <> show err))
   Right jsonSpec -> liftEffect $ loadFigureFromJsonSpec jsonSpec

loadFigure :: String -> Effect Unit
loadFigure filename = launchAff_ do
   jsonSpec <- loadSpec filename
   liftEffect $ loadFigureFromJsonSpec jsonSpec

drawCode :: String -> String -> Effect Unit
drawCode folder file = runAffs_ drawFile
   [ runWebT (FileCxt { fluidSrcPaths: [ Folder folder ] }) $ loadFile' [ Folder folder ] (File file)
   ]