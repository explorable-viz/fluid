module App.View.TableView where

import Prelude

import App.Util (Handler, Renderer, Selector, 𝕊, SelState, selector, unsafeEventData)
import App.Util.Selector (field, listElement)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (uncurry)
import Dict (Dict)
import Util (type (×), (×))
import Val (Val)
import Web.Event.Event (EventType, target, type_)
import Web.Event.Internal.Types (EventTarget)

newtype TableView = TableView
   { title :: String
   , filter :: Boolean
   , table :: Array (Dict (Val (SelState 𝕊)))
   }

foreign import drawTable :: Renderer TableView

tableViewHandler :: Handler
tableViewHandler = (target &&& type_) >>> pos >>> uncurry \(n × x) -> listElement n <<< field x
   where
   -- [Unsafe] 0-based index of selected record and name of field.
   pos :: Maybe EventTarget × EventType -> (Int × String) × Selector Val
   pos (tgt_opt × ty) = ((cell.__n - 1) × cell.name) × selector ty
      where
      -- first field name must equal indexKey in TableView.js
      cell :: { __n :: Int, name :: String }
      cell = unsafeEventData tgt_opt
