module App.Main where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import App.Renderer (Fig, Fig', FigSpec, LinkingFigSpec, drawFig, drawFig', loadFig, loadLinkingFig)
import Module (File(..))
import Test.Util (selectBarChart_data, selectCell, selectNth, select_y)

linkingFig1 :: LinkingFigSpec
linkingFig1 = {
   divId: "fig-1",
   config: {
      file1: File "bar-chart",
      file2: File "line-chart",
      dataFile: File "renewables",
      dataVar: "data",
      v1_sel: selectBarChart_data (selectNth 1 (select_y))
   }
}

fig1 :: FigSpec
fig1 = {
   divId: "fig-conv-1",
   file: File "slicing/conv-emboss",
   vars: ["image", "filter"]
}

drawFigs :: forall r . Array (Aff (Fig r)) -> Effect Unit
drawFigs loadFigs =
   flip runAff_ (sequence loadFigs)
   case _ of
      Left err -> log $ show err
      Right figs -> sequence_ $ drawFig <$> figs

drawFigs' :: Array (Aff Fig') -> Effect Unit
drawFigs' loadFigs =
   flip runAff_ (sequence loadFigs)
   case _ of
      Left err -> log $ show err
      Right figs -> sequence_ $ flip drawFig' (selectCell 2 2 5 5) <$> figs

main :: Effect Unit
main = do
   drawFigs' [loadFig fig1]
   drawFigs [loadLinkingFig linkingFig1]
