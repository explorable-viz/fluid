module App.Main where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import App.Fig (Fig, FigSpec, LinkFig', LinkFigSpec, drawFig, drawLinkFig, loadFig, loadLinkFig)
import App.Util (selectBarChart_data, selectNth, select_y)
import Module (File(..))
import Val (Val(..))

linkingFig1 :: LinkFigSpec
linkingFig1 = {
   divId: "fig-1",
   file1: File "bar-chart",
   file2: File "line-chart",
   dataFile: File "renewables",
   dataVar: "data",
   v1_sel: selectBarChart_data (selectNth 1 (select_y))
}

fig1 :: FigSpec
fig1 = {
   divId: "fig-conv-1",
   file: File "slicing/conv-emboss",
   vars: ["image", "filter"]
}

drawLinkFigs :: Array (Aff LinkFig') -> Effect Unit
drawLinkFigs loadFigs =
   flip runAff_ (sequence loadFigs)
   case _ of
      Left err -> log $ show err
      Right figs -> sequence_ $ flip drawLinkFig (selectBarChart_data (selectNth 1 (select_y))) <$> figs

drawFigs :: Array (Aff Fig) -> Effect Unit
drawFigs loadFigs =
   flip runAff_ (sequence loadFigs)
   case _ of
      Left err -> log $ show err
      Right figs -> sequence_ $ flip drawFig (Hole false) <$> figs

main :: Effect Unit
main = do
   drawFigs [loadFig fig1]
   drawLinkFigs [loadLinkFig linkingFig1]
