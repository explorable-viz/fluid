module Test.App where

import Prelude

import App (fig1)
import App.Fig (FigSpec, LinkedOutputsFigSpec, loadFig, loadLinkedOutputsFig)
import Effect.Aff (Aff)
import Util (type (×), (×))

-- For now just exercise figure creation code; will test via UI at some point.
test_fig :: FigSpec -> String × Aff Unit
test_fig spec = spec.divId × void (loadFig spec)

test_linkedOutputsFig :: LinkedOutputsFigSpec -> String × Aff Unit
test_linkedOutputsFig spec = spec.divId × void (loadLinkedOutputsFig spec)

app_tests :: Array (String × Aff Unit)
app_tests = [ test_fig fig1 ]
