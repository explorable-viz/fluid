module App.Main where

import Prelude hiding (absurd)

import App.CodeMirror (addEditorView)
import App.Fig (Fig, FigSpec, LinkFig, LinkFigSpec, drawCode, drawFig, drawLinkFig, loadFig, loadLinkFig)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Lattice (botOf)
import Module (File(..), Folder(..), loadFile)
import Util ((×), type (×), error)

linkingFig1 :: LinkFigSpec
linkingFig1 =
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

drawLinkFigs :: Array (Aff LinkFig) -> Effect Unit
drawLinkFigs loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> error $ show err
         Right figs ->
            sequence_ $ figs <#>
               ( \fig -> do
                    e1 <- addEditorView ("codemirror-" <> unwrap fig.spec.file1)
                    e2 <- addEditorView ("codemirror-" <> unwrap fig.spec.file2)
                    e3 <- addEditorView ("codemirror-" <> unwrap fig.spec.dataFile)
                    drawLinkFig fig e1 e2 e3 (Left $ botOf)
               )

drawFigs :: Array (Aff Fig) -> Effect Unit
drawFigs loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> log $ show err
         Right figs ->
            sequence_ $ figs <#> \fig -> do
               ed <- addEditorView ("codemirror-" <> fig.spec.divId)
               drawFig fig ed botOf

drawFiles :: Array (Folder × File) -> Effect Unit
drawFiles arr = sequence_ $ arr <#> \(folder × file@(File name)) ->
   flip runAff_ (loadFile folder file)
      case _ of
         Left err -> log $ show err
         Right fileconts -> do
            ed <- addEditorView ("codemirror-" <> name)
            drawCode ed fileconts

main :: Effect Unit
main = do
   drawFiles [ Folder "fluid/lib" × File "convolution" ]
   drawFigs [ loadFig fig1, loadFig fig2 ]
   drawLinkFigs [ loadLinkFig linkingFig1 ]
