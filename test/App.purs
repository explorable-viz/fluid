module Test.App where

import Prelude
import App (fig1, fig2, linkedOutputsFig1)
import App.Fig (FigSpec, LinkedOutputsFigSpec, loadFig, loadLinkedOutputsFig)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Util.Mocha (run)
import Util (type (×), (×))

-- For now just exercise figure creation code; will test via UI at some point.
test_fig :: FigSpec -> String × Aff Unit
test_fig spec = spec.divId × void (loadFig spec)

test_linkedOutputsFig :: LinkedOutputsFigSpec -> String × Aff Unit
test_linkedOutputsFig spec = spec.divId × void (loadLinkedOutputsFig spec)

tests :: Array (String × Aff Unit)
tests = [ test_fig fig1, test_fig fig2, test_linkedOutputsFig linkedOutputsFig1 ]

main :: Effect Unit
main = run tests
