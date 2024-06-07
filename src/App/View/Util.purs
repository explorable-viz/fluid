module App.View.Util where

import Prelude

import App.Util (SelState, Selectable, 𝕊)
import Dict (Dict)
import Effect (Effect)
import Util (Endo)
import Val (BaseVal, Val)
import Web.Event.EventTarget (EventListener)

type HTMLId = String

-- Heavily curried type isn't convenient for FFI
type RendererSpec a =
   { uiHelpers :: UIHelpers
   , divId :: HTMLId
   , suffix :: String
   , view :: a
   }

type Renderer a = RendererSpec a -> EventListener -> Effect Unit

-- Bundle into a record so we can export via FFI
type UIHelpers =
   { val :: forall a. Selectable a -> a
   , selState :: forall a. Selectable a -> SelState 𝕊
   , join :: SelState 𝕊 -> SelState 𝕊 -> SelState 𝕊
   , selClasses :: String
   , selClass :: SelState 𝕊 -> String
   , barChart ::
        { bar_fill :: SelState 𝕊 -> Endo String
        , bar_stroke :: SelState 𝕊 -> Endo String
        }
   , lineChart ::
        { point_smallRadius :: Int
        , point_radius :: SelState 𝕊 -> Int
        , point_stroke :: SelState 𝕊 -> Endo String
        }
   , tableView ::
        { rowKey :: String
        , record_isUsed :: Dict (Val (SelState 𝕊)) -> Boolean
        , cell_selClass :: String -> SelState 𝕊 -> String
        -- values in table cells are not "unpacked" to Selectable but remain as Val
        , val_val :: Val (SelState 𝕊) -> BaseVal (SelState 𝕊)
        , val_selState :: Val (SelState 𝕊) -> SelState 𝕊
        }
   }
