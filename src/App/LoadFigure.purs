module App.LoadFigure where

import Prelude hiding (absurd)

import Affjax.ResponseFormat (json)
import Affjax.Web (get, printError)
import App.Fig (drawFig, drawFile, loadFig)
import App.Util (runAffs_)
import App.View.Util (FigSpec)
import Bind (Bind)
import Data.Argonaut.Decode (decodeJson)
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

loadFigure :: String -> Effect Unit -- TO DELETE
loadFigure fileName = runAffs_ (uncurry drawFig)
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

loadFigure_ :: FigSpec -> Effect Unit -- TO DO: RENAME TO loadFigure once the old one is gone 
loadFigure_ spec@{ fluidSrcPaths } = runAffs_ (uncurry drawFig)
   [ do
        ("fig" × _) <$> runWebT (FileCxt { fluidSrcPaths }) (loadFig spec)
   ]

loadSpec :: String -> Aff (Maybe JsonSpec)
loadSpec filename = do
   result <- get json filename
   case result of
      Left err -> error ("Json fetching failed with " <> printError err)
      Right response ->
         case decodeJson response.body of
            Left err -> error ("JSON decoding failed with " <> show err)
            Right spec -> pure $ Just spec

loadFigureOLD :: String -> Effect Unit --OLD WAY OF DOING IT, SHOULD STILL WORK
loadFigureOLD filename = launchAff_ do
   jsonSpec' <- loadSpec filename
   case jsonSpec' of
      Just jsonSpec -> do
         let figSpec = figSpecFromJson jsonSpec
         liftEffect $ loadFigure_ figSpec
      Nothing -> do
         pure unit

drawCode :: String -> String -> Effect Unit
drawCode folder file = runAffs_ drawFile
   [ runWebT (FileCxt { fluidSrcPaths: [ Folder folder ] }) $ loadFile' [ Folder folder ] (File file)
   ]
