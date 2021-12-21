module App.TableView where

import Prelude
import Data.Tuple (fst)
import Effect.Console (log)
import Bindings (Bindings)
import App.Util (Handler, Renderer, get_intOrNumber, get_prim)
import Lattice (𝔹)
import Primitive (Slice)
import Util (type (×))
import Val (Val)

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to a single specific example for now.
type EnergyRecord = { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
newtype EnergyTable = EnergyTable { title :: String, table :: Array EnergyRecord }

foreign import drawTable :: Renderer EnergyTable

energyRecord :: Slice (Bindings (Val 𝔹)) -> EnergyRecord
energyRecord r = {
   year: get_prim "year" r,
   country: get_prim "country" r,
   energyType: get_prim "energyType" r,
   output: get_intOrNumber "output" r
}

tableViewHandler :: Handler
tableViewHandler redraw ev = do
   log $ show "tableViewHandler"
   redraw fst
