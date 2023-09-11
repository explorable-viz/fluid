module App.Main where

import Prelude hiding (absurd)

import App.CodeMirror (EditorView, addEditorView)
import App.Fig (Fig, FigSpec, LinkFig, LinkFigSpec, drawCode, drawFig, drawLinkFig, loadFig, loadLinkFig)
import Data.Either (Either(..))
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Lattice (botOf)
import Module (File(..), Folder(..), loadFile)
import Util ((×), type (×))
import Util.Triple (Triple(..))

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

-- drawLinkFigs :: Array (Aff LinkFig) -> Effect Unit
-- drawLinkFigs loadFigs =
--    flip runAff_ (sequence loadFigs)
--       case _ of
--          Left err -> log $ show err
--          Right figs -> do
--             ed1 <- addEditorView "codemirror-barchart"
--             ed2 <- addEditorView "codemirror-linechart"
--             ed3 <- addEditorView "empty"
--             sequence_ $ (\fig -> drawLinkFig fig ((ed1 × ed2)× ed3) (Left $ botOf)) <$> figs 
-- (\fig -> do 
                                   -- let editorviews = getEditorViews fig
                                    --drawLinkFig fig editorviews (Left $ botOf) <$> figs)

drawLinkFigs :: Array (Aff LinkFig)  -> Effect Unit
drawLinkFigs loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> log $ show err
         Right figs -> do
            sequence_ $ (\fig -> do
                        Triple e1 e2 e3 <- getEditorViews fig
                        drawLinkFig fig (Triple e1 e2 e3) (Left $ botOf)
                        ) <$> figs 

getEditorViews :: LinkFig -> Effect (Triple EditorView)
getEditorViews {spec:{file1:File file1, file2:File file2, dataFile:File dataset}} = sequence $ addEditorView <$> (Triple ("codemirror-" <> file1)  ("codemirror-" <> file2)   ("codemirror-" <> dataset))

-- drawFigs :: Array (Aff Fig) -> Effect Unit
-- drawFigs loadFigs =
--    flip runAff_ (sequence loadFigs)
--       case _ of
--          Left err -> log $ show err
--          Right figs -> sequence_ $ flip drawFig botOf <$> figs

drawFigs :: Array (Aff Fig) -> Effect Unit
drawFigs loadFigs =
   flip runAff_ (sequence loadFigs)
      case _ of
         Left err -> log $ show err
         Right figs -> do
           sequence_ $ (\fig -> drawFigHelper fig) <$> figs

drawFigHelper :: Fig -> Effect Unit
drawFigHelper fig =
            do
            ed <- addEditorView ("codemirror-" <> fig.spec.divId)
            drawFig fig ed botOf

drawFiles :: Array (Folder × File) -> Effect Unit 
drawFiles arr = sequence_ $ drawFile <$> arr 

drawFile :: (Folder × File) -> Effect Unit 
drawFile (fol × fil@(File name)) = 
   let conts = loadFile fol fil in 
      do
        flip runAff_ conts 
         case _ of 
            Left err -> log $ show err 
            Right fileconts -> do
              ed <- addEditorView ("codemirror-" <> name)
              drawCode ed fileconts  



main :: Effect Unit
main = do
   drawFiles [(Folder "fluid/lib") × (File "convolution")]
   drawFigs [ loadFig fig1, loadFig fig2 ]
   drawLinkFigs [ loadLinkFig linkingFig1 ]
   