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
import Effect.Aff (Aff, launchAff_)
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

loadSpec :: String -> Aff Json
loadSpec filename = do
   result <- get json filename
   case result of
      Left err -> error ("Json fetching failed with " <> printError err)
      Right response -> pure $ response.body

loadFigureFromJson :: Json -> Effect Unit
loadFigureFromJson json = runAffs_ (uncurry drawFig)
   [ case decodeJson json :: Either JsonDecodeError JsonSpec of
        Left err -> error ("JSON decoding failed with " <> show err)
        Right spec -> ("fig" × _) <$> runWebT (FileCxt { fluidSrcPaths }) (loadFig figSpec)
           where
           figSpec@{ fluidSrcPaths } = figSpecFromJson spec
   ]

loadFigure :: String -> Effect Unit
loadFigure filename = launchAff_ do
   jsonSpec <- loadSpec filename
   liftEffect $ loadFigureFromJson jsonSpec

drawCode :: String -> String -> Effect Unit
drawCode folder file = runAffs_ drawFile
   [ runWebT (FileCxt { fluidSrcPaths: [ Folder folder ] }) $ loadFile' [ Folder folder ] (File file)
   ]
   
