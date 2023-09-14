module Test.App.Main where

import Prelude

import App.Fig (FigSpec, LinkFigSpec, loadFig, loadLinkFig)
import App.Main (fig1, fig2, linkingFig1)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.TestRunners (run)
import Util (type (×), (×))

-- For now app tests just exercise figure creation code.
test_fig :: FigSpec -> String × Aff Unit
test_fig spec = spec.divId × void (loadFig spec)

test_linkingFig :: LinkFigSpec -> String × Aff Unit
test_linkingFig spec = spec.divId × void (loadLinkFig spec)

tests :: Array (String × Aff Unit)
tests = [ test_fig fig1, test_fig fig2, test_linkingFig linkingFig1 ]

main :: Effect Unit
main = run tests
