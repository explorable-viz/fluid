module App.LoadFigure where

import Prelude hiding (absurd)

import Affjax.ResponseFormat (json)
import Affjax.Web (get, printError)
import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind (Bind)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Doc (DocOpt(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
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
loadFigureFromSpec encodedJsonSpec = case decodeJson encodedJsonSpec :: Either JsonDecodeError JsonSpec of
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