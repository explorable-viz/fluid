module App.LoadFigure where

import Prelude hiding (absurd)

import Affjax.ResponseFormat (json)
import Affjax.Web (get, printError)
import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import App.View.Util (Options)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Array (head, last)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import File (File(..), FileCxt(..), Folder(..), loadFileFromPath)
import Graph (DVertex'(..))
import Module.Web (runWebT)
import Util (definitely, definitely', error, (×))
import Val (Val(..), asVal)

type JsonSpec =
   { fluidSrcPath :: Array String
   , inputs :: Array String
   , query :: Boolean
   , linking :: Boolean
   }

figSpecFromJson :: JsonSpec -> Options
figSpecFromJson spec@{ inputs, query, linking } =
   { fluidSrcPaths: Folder <$> spec.fluidSrcPath
   , inputs
   , query:
        if query then
           Just $ asVal >=> case _ of
              v@(Val α (Just _) _) -> Just $ DVertex (α × v)
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

loadFigure :: Json -> String -> Effect Unit
loadFigure jsonSpec srcFile = launchAff_ do
   fluidSrc <- loadFileFromPath (File srcFile)
   liftEffect $ loadFigureSrc jsonSpec (definitely' fluidSrc)

loadFigureSrc :: Json -> String -> Effect Unit
loadFigureSrc jsonSpec fluidSrc = runAffs_ (uncurry drawFig)
   [ case decodeJson jsonSpec :: Either JsonDecodeError JsonSpec of
        Left err -> error ("JSON decoding failed with " <> show err)
        Right spec -> do
           let figSpec@{ fluidSrcPaths } = figSpecFromJson spec
           ("fig" × _) <$> runWebT (FileCxt { fluidSrcPaths }) (loadFig figSpec fluidSrc)
   ]

loadCode :: String -> Effect Unit
loadCode file = launchAff_ do
   fluidSrc <- loadFileFromPath (File file)
   liftEffect $ drawFile (File filename × definitely ("loadCode: File not found: " <> file) fluidSrc)
   where
   filename :: String
   filename = definitely ("loadCode: Filename cannot be empty: " <> file) do
      splitPath <- last (split (Pattern "/") file)
      filename_ <- head (split (Pattern ".") splitPath)
      if filename_ == "" then Nothing else pure filename_
