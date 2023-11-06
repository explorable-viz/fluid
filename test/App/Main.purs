module Test.App.Main where

import Prelude
import App.Fig (FigSpec, LinkedOutputsFigSpec, loadFig, loadLinkedOutputsFig)
import App.Main (fig1, fig2, linkingFig1)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Util.Mocha (run)
import Util (type (×), (×))

-- For now app tests just exercise figure creation code.
test_fig :: FigSpec -> String × Aff Unit
test_fig spec = spec.divId × void (loadFig spec)

test_linkedOutputsFig :: LinkedOutputsFigSpec -> String × Aff Unit
test_linkedOutputsFig spec = spec.divId × void (loadLinkedOutputsFig spec)

tests :: Array (String × Aff Unit)
tests = [ test_fig fig1, test_fig fig2, test_linkedOutputsFig linkingFig1 ]

main :: Effect Unit
main = run tests
