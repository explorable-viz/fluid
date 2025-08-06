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
import File (File(..), FileCxt(..), Folder(..), loadFileFromPath)
import Graph (DVertex'(..))
import Module.Web (loadFile_, runWebT)
import Util (error, (×))
import Val (Val(..), asVal)

type JsonSpec =
   { fluidSrcPath :: Array String
   , datasets :: Array (Bind String)
   , file :: String
   , inputs :: Array String
   , query :: Boolean
   , linking :: Boolean
   }

figSpecFromJson :: JsonSpec -> FigSpec
figSpecFromJson spec@{ datasets, file, inputs, query, linking } =
   { fluidSrcPaths: Folder <$> spec.fluidSrcPath
   , datasets
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

--loadFigure -- specFile (String) & srcFile (String)
loadFigure :: String -> String -> Effect Unit
loadFigure specFile srcFile = launchAff_ do
   jsonSpec <- loadSpec specFile
   fluidSrc_ <- loadFileFromPath (File srcFile)
   case fluidSrc_ of
      Nothing -> error ("File not found: " <> show srcFile)
      Just fluidSrc -> liftEffect $ loadFigureSpecSrc jsonSpec fluidSrc

--loadFigureSrc -- specFile (String) & fluidSrc (String)
loadFigureSrc :: String -> String -> Effect Unit
loadFigureSrc specFile fluidSrc = launchAff_ do
   jsonSpec <- loadSpec specFile
   liftEffect $ loadFigureSpecSrc jsonSpec fluidSrc

--loadFigureSpec -- jsonSpec (Json) & srcFile (String)
loadFigureSpec :: Json -> String -> Effect Unit
loadFigureSpec jsonSpec srcFile = launchAff_ do
   fluidSrc_ <- loadFileFromPath (File srcFile)
   case fluidSrc_ of
      Nothing -> error ("File not found: " <> show srcFile)
      Just fluidSrc -> liftEffect $ loadFigureSpecSrc jsonSpec fluidSrc

--loadFigureSpecSrc -- jsonSpec (Json) & fluidSrc (String)
--PS: all 4 versions pass the parameters to loadFigureFromRawValues
loadFigureSpecSrc :: Json -> String -> Effect Unit
loadFigureSpecSrc jsonSpec fluidSrc = runAffs_ (uncurry drawFig)
   [ case decodeJson jsonSpec :: Either JsonDecodeError JsonSpec of
        Left err -> error ("JSON decoding failed with " <> show err)
        Right spec -> do
           let figSpec@{ fluidSrcPaths } = figSpecFromJson spec
           ("fig" × _) <$> runWebT (FileCxt { fluidSrcPaths }) (loadFig figSpec fluidSrc)
   ]

drawCode :: String -> String -> Effect Unit
drawCode folder file = runAffs_ drawFile
   [ runWebT (FileCxt { fluidSrcPaths: [ Folder folder ] }) $ loadFile_ [ Folder folder ] (File file)
   ]

