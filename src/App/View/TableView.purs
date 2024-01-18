module App.View.TableView where

import Prelude

import App.Util (Handler, Renderer, Sel, unsafeEventData)
import App.Util.Selector (field, listElement)
import Data.Maybe (Maybe)
import Dict (Dict)
import Lattice (neg)
import Util (type (×), (×))
import Val (Val)
import Web.Event.Event (target)
import Web.Event.Internal.Types (EventTarget)

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   , table :: Array (Dict (Val Sel))
   }

foreign import drawTable :: Renderer TableView

tableViewHandler :: Handler
tableViewHandler = target >>> pos >>> \(n × x) -> listElement n (field x neg)
   where
   -- [Unsafe] 0-based index of selected record and name of field.
   pos :: Maybe EventTarget -> Int × String
   pos tgt_opt = (cell.__n - 1) × cell.name
      where
      -- first field name must equal indexKey in TableView.js
      cell :: { __n :: Int, name :: String }
      cell = unsafeEventData tgt_opt
