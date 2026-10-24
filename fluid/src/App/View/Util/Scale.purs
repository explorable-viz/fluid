module App.View.Util.Scale where

import Prelude

import Data.Array (range)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..))

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

data TickSpacing
   = AutoTicks
   | Count Int
   | Every Number

data LabelFormat
   = AutoFormat
   | Decimals Int

newtype AxisTicks = AxisTicks { spacing :: TickSpacing, format :: LabelFormat }

newtype Ticks = Ticks { x :: AxisTicks, y :: AxisTicks }

-- Tick values within domain; Nothing when d3 chooses
resolveTicks :: TickSpacing -> Interval -> Maybe (Array Number)
resolveTicks (Every step) { min, max } = Just (if first > last then [] else range first last <#> toNumber >>> (_ * step))
   where
   first = ceil (min / step - tolerance)
   last = floor (max / step + tolerance)
   -- Round-off in quotient
   tolerance = 1e-9
resolveTicks _ _ = Nothing

-- Count hint for d3, given default
tickCount :: TickSpacing -> Number -> Number
tickCount (Count n) _ = toNumber n
tickCount _ default = default

decimalPlaces :: LabelFormat -> Maybe Int
decimalPlaces (Decimals places) = Just places
decimalPlaces AutoFormat = Nothing

-- ======================
-- boilerplate
-- ======================

derive instance Eq Scale
