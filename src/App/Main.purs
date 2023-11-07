module App.Main where

import Prelude hiding (absurd)

import App.CodeMirror (addEditorView)
import App.Fig (Fig, FigSpec, LinkedOutputsFig, LinkedOutputsFigSpec, drawCode, drawFig, drawLinkedOutputsFig, loadFig, loadLinkedOutputsFig)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Lattice (botOf)
import Module (File(..), Folder(..), loadFile)
import Util (type (×), (×), Endo)

linkedOutputsFig1 :: LinkedOutputsFigSpec
linkedOutputsFig1 =
   { divId: "fig-1"
   , file1: File "bar-chart"
   , file2: File "line-chart"
   , dataFile: File "renewables"
   , x: "data"
   }

fig1 :: FigSpec
fig1 =
   { divId: "fig-conv-1"
   , file: File "slicing/convolution/emboss"
   , xs: [ "image", "filter" ]
   }

fig2 :: FigSpec
fig2 =
   { divId: "fig-conv-2"
   , file: File "slicing/convolution/emboss-wrap"
   , xs: [ "image", "filter" ]
   }

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

drawLinkedOutputsFigs :: Array (Aff LinkedOutputsFig) -> Effect Unit
drawLinkedOutputsFigs loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> log $ show err
         Right figs -> do
            sequence_ $ figs <#> \fig -> do
               ed1 <- addEditorView $ codeMirrorDiv $ unwrap (fig.spec.file1)
               ed2 <- addEditorView $ codeMirrorDiv $ unwrap (fig.spec.file2)
               ed3 <- addEditorView $ codeMirrorDiv $ unwrap (fig.spec.dataFile)
               drawLinkedOutputsFig fig ed1 ed2 ed3 (Left $ botOf)

drawFigs :: Array (Aff Fig) -> Effect Unit
drawFigs loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> log $ show err
         Right figs -> sequence_ $ figs <#> \fig -> do
            ed <- addEditorView $ codeMirrorDiv fig.spec.divId
            drawFig fig ed botOf

drawFiles :: Array (Folder × File) -> Effect Unit
drawFiles files =
   sequence_ $ files <#> \(folder × file) ->
      flip runAff_ (loadFile folder file)
         case _ of
            Left err -> log $ show err
            Right src -> do
               ed <- addEditorView $ codeMirrorDiv $ unwrap file
               drawCode ed src

main :: Effect Unit
main = do
   drawFiles [ Folder "fluid/lib" × File "convolution" ]
   drawFigs [ loadFig fig1, loadFig fig2 ]
   drawLinkedOutputsFigs [ loadLinkedOutputsFig linkedOutputsFig1 ]
