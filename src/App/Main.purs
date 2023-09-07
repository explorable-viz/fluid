module App.Main where

import Prelude hiding (absurd)

import App.CodeMirror (addEditorView)
import App.Fig (Fig, FigSpec, LinkFig, LinkFigSpec, drawFig, drawFigTemp, drawLinkFig, loadFig, loadLinkFig)
import Data.Either (Either(..))
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Lattice (botOf)
import Module (File(..))
import Util.Pair (Pair(..))

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
         Left err -> log $ show err
         Right figs -> do
            ed1 <- addEditorView "codemirror-barchart"
            ed2 <- addEditorView "codemirror-linechart"
            sequence_ $ (\fig -> drawLinkFig fig (Pair ed1 ed2) (Left $ botOf)) <$> figs

drawFigs :: Array (Aff Fig) -> Effect Unit
drawFigs loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> log $ show err
         Right figs -> sequence_ $ flip drawFig botOf <$> figs

drawFigsTemp :: Array (Aff Fig) -> Effect Unit
drawFigsTemp loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> log $ show err
         Right figs -> do
           -- ed <- addEditorView "codemirror-data"
           sequence_ $ (\fig -> drawFigsTemp2 fig) <$> figs

drawFigsTemp2 :: Fig -> Effect Unit
drawFigsTemp2 fig =
            do
            ed <- addEditorView ("codemirror-" <> fig.spec.divId)
            drawFigTemp fig ed botOf 


main :: Effect Unit
main = do
   drawFigsTemp [ loadFig fig1, loadFig fig2 ]
   drawLinkFigs [ loadLinkFig linkingFig1 ]
