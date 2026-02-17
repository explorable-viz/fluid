module App.View.Util.Point where

import App.Util (Selectable)
import App.View.Util.D3 (Coord)
import Data.Newtype (class Newtype)

newtype Point a = Point (Coord (Selectable a))

-- ======================
-- boilerplate
-- ======================

derive instance Newtype (Point a) _
