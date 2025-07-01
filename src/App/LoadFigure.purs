-- Should this be src/Αpp rather than website/Website.LoadFigure?
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
import Graph (DVertex'(..))
import Module.Web (File(..), Folder(..), loadFile')
import Util (error, (×))
import Val (Val(..), asVal)

type JsonSpec =
   { fluidSrcPath :: Array String
   , datasets :: Array (Bind String)
   , imports :: Array String
   , file :: String
   , inputs :: Array String
   , query :: Boolean
   }

figSpecFromJson :: JsonSpec -> FigSpec
figSpecFromJson spec =
   { fluidSrcPaths: Folder <$> spec.fluidSrcPath
   , datasets: spec.datasets
   , imports: spec.imports
   , file: File spec.file
   , inputs: spec.inputs
   , query:
        if spec.query then
           Just $ asVal >=> case _ of
              v@(Val α (Doc _ _) _) -> Just $ DVertex (α × v)
              _ -> Nothing
        else Nothing
   }

loadFigure :: String -> Effect Unit
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
                    ("fig" × _) <$> loadFig (figSpecFromJson spec)
   ]

drawCode :: String -> String -> Effect Unit
drawCode folder file = runAffs_ drawFile
   [ loadFile' [ Folder folder ] (File file)
   ]
