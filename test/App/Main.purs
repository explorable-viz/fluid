module Test.App.Main where

import Prelude

import App.Fig (FigSpec, LinkFigSpec, loadFig, loadLinkFig)
import App.Main (fig1, fig2, linkingFig1)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec.Mocha (run)
import Util (type (×), (×))

-- For now app tests just exercise figure creation code.
test_fig :: FigSpec -> String × Aff Unit
test_fig spec =
   let
      _ = void (loadFig spec)
   in
      (spec.divId × pure unit)

test_linkingFig :: LinkFigSpec -> String × Aff Unit
test_linkingFig spec =
   let
      _ = void (loadLinkFig spec)
   in
      (spec.divId × pure unit)

tests :: Array (String × Aff Unit)
tests = [ test_fig fig1, test_fig fig2, test_linkingFig linkingFig1 ]

main :: Effect Unit
main = run tests
