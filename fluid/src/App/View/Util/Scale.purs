module App.View.Util.Scale where

import Prelude

type Interval = { min :: Number, max :: Number }

data Scale
   = Auto
   | FromZero
   | Fixed Number Number

newtype Scales = Scales { x :: Scale, y :: Scale }

-- Domain of axis, given data range
resolveScale :: Scale -> Interval -> Interval
resolveScale Auto range = range
resolveScale FromZero range = { min: 0.0, max: range.max }
resolveScale (Fixed min max) _ = { min, max }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Scale
