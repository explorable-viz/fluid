module App.View.TableView where

import Prelude

import App.Util (Handler, Renderer, Sel)
import App.Util.Selector (field, listElement)
import Data.Maybe (Maybe)
import Dict (Dict)
import Lattice (neg)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (×), definitely')
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
tableViewHandler = target >>> unsafePos >>> \(n × x) -> listElement n (field x neg)
   where
   -- [Unsafe] 0-based index of selected record and name of field.
   unsafePos :: Maybe EventTarget -> Int × String
   unsafePos tgt_opt = (cell.__n - 1) × cell.name
      where
      -- first field name must equal indexKey in TableView.purs
      cell :: { __n :: Int, name :: String }
      cell = (unsafeCoerce $ definitely' tgt_opt).__data__
