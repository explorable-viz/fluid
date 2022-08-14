module App.TableView2 where

import Prelude
import Bindings2 (Bindings)
import App.Util2 (Handler, Renderer, get_intOrNumber, get_prim)
import Lattice2 (𝔹)
import Util2 (type (×))
import Val2 (Val)

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to a single specific example for now.
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
newtype EnergyTable = EnergyTable { title :: String, table :: Array EnergyRecord }

foreign import drawTable :: Renderer EnergyTable

energyRecord :: Bindings (Val 𝔹) -> EnergyRecord
energyRecord r = {
   year: get_prim "year" r,
   country: get_prim "country" r,
   energyType: get_prim "energyType" r,
   output: get_intOrNumber "output" r
}

tableViewHandler :: Handler
tableViewHandler = const identity
