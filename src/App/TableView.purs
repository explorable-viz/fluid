module App.TableView where

import Prelude
import App.Util (Handler, Renderer, get_intOrNumber)
import Dict (Dict, get)
import Lattice (𝔹)
import Primitive (int, string)
import Util (type (×))
import Val (Val)

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to a single specific example for now.
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
newtype EnergyTable = EnergyTable { title :: String, table :: Array EnergyRecord }

foreign import drawTable :: Renderer EnergyTable

energyRecord :: Dict (Val 𝔹) -> EnergyRecord
energyRecord r =
   { year: int.match (get "year" r)
   , country: string.match (get "country" r)
   , energyType: string.match (get "energyType" r)
   , output: get_intOrNumber "output" r
   }

tableViewHandler :: Handler
tableViewHandler = const identity
