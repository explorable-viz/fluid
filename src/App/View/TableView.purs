module App.View.TableView where

import Prelude

import App.Util (Handler, Renderer, Sel)
import App.Util.Selector (field, listElement)
import Data.Maybe (Maybe)
import Dict (Dict)
import Lattice (neg)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (×), definitely', spy)
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
   unsafePos tgt_opt = 0 × cell.name
      where
      -- Not sure how to get heterogeneous tuple from JS, so resort to this abomination.
      cell = spy "event data" identity (unsafeCoerce $ definitely' tgt_opt).__data__ :: { name :: String }
