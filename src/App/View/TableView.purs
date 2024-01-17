module App.View.TableView where

import Prelude

import App.Util (Handler, Renderer, Sel)
import Data.Maybe (Maybe)
import Dict (Dict)
import Unsafe.Coerce (unsafeCoerce)
import Util (definitely', spy, (!))
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
tableViewHandler = target >>> unsafePos >>> const identity
   where
   -- [Unsafe] Datum associated with matrix view mouse event; 1-based indices of selected cell.
   unsafePos :: Maybe EventTarget -> String
   unsafePos tgt_opt = x
      where
      x = (unsafeCoerce $ definitely' (spy "event data" identity tgt_opt)).__data__ ! 0 :: String
