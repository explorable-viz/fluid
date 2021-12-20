module App.Main where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import App.Renderer (FigSpec, LinkingFigSpec, drawFig, loadFig, loadLinkingFig)
import Module (File(..))
import Test.Util (selectBarChart_data, selectNth, select_y)


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

main :: Effect Unit
main = 
   flip runAff_ (sequence [loadFig fig1, loadLinkingFig linkingFig1])
   case _ of
      Left err -> log $ show err
      Right figs -> sequence_ $ drawFig <$> figs
