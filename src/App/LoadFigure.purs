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
import File (File(..), FileCxt(..), Folder(..), fluidExtension, loadFileFromPath)
import Graph (DVertex'(..))
import Module.Web (runWebT)
import Util (definitely', error, (×))
import Val (Val(..), asVal)

type JsonSpec =
   { fluidSrcPath :: Array String
   , datasets :: Array (Bind String)
   , inputs :: Array String
   , query :: Boolean
   , linking :: Boolean
   }

figSpecFromJson :: JsonSpec -> FigSpec
figSpecFromJson spec@{ datasets, inputs, query, linking } =
   { fluidSrcPaths: Folder <$> spec.fluidSrcPath
   , datasets
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

--loadFigure -- specFilename (String) & srcFilename (String)
loadFigure :: String -> String -> Effect Unit
loadFigure specFile srcFile = launchAff_ do
   jsonSpec <- loadSpec specFile
   liftEffect $ loadFigureSpec jsonSpec srcFile

--loadFigureSrc -- specFilename (String) & fluidSrc (String)
loadFigureSrc :: String -> String -> Effect Unit
loadFigureSrc specFile fluidSrc = launchAff_ do
   jsonSpec <- loadSpec specFile
   liftEffect $ loadFigureSpecSrc jsonSpec fluidSrc

--loadFigureSpec -- jsonSpec (Json) & srcFilename (String)
loadFigureSpec :: Json -> String -> Effect Unit
loadFigureSpec jsonSpec srcFile = launchAff_ do
   fluidSrc <- loadFileFromPath (File (srcFile <> fluidExtension))
   liftEffect $ loadFigureSpecSrc jsonSpec (definitely' fluidSrc)

--loadFigureSpecSrc -- jsonSpec (Json) & fluidSrc (String)
loadFigureSpecSrc :: Json -> String -> Effect Unit
loadFigureSpecSrc jsonSpec fluidSrc = runAffs_ (uncurry drawFig)
   [ case decodeJson jsonSpec :: Either JsonDecodeError JsonSpec of
        Left err -> error ("JSON decoding failed with " <> show err)
        Right spec -> do
           let figSpec@{ fluidSrcPaths } = figSpecFromJson spec
           ("fig" × _) <$> runWebT (FileCxt { fluidSrcPaths }) (loadFig figSpec fluidSrc)
   ]

drawCode :: String -> Effect Unit
drawCode file = launchAff_ do
   fluidSrc <- loadFileFromPath (File (file <> fluidExtension))
   liftEffect $ drawFile ((File file) × (definitely' fluidSrc))

