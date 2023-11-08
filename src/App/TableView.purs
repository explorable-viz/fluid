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

type WaterRecord = {country :: String × 𝔹, cities :: Int × 𝔹, farms :: Int × 𝔹, industry :: Int × 𝔹, energy :: Int × 𝔹, popMil :: Int × 𝔹}
newtype WaterTable = WaterTable { title :: String, table :: Array WaterRecord}


foreign import drawTable :: Renderer WaterTable

waterRecord :: Dict (Val 𝔹) -> WaterRecord
waterRecord r = 
   { country: string.unpack (get "country" r)
   , cities: int.unpack (get "cities" r)
   , farms: int.unpack (get "farms" r)
   , industry: int.unpack (get "industry" r)
   , energy: int.unpack (get "energy" r)
   , popMil: int.unpack (get "popMil" r)
   }

energyRecord :: Dict (Val 𝔹) -> EnergyRecord
energyRecord r =
   { year: int.unpack (get "year" r)
   , country: string.unpack (get "country" r)
   , energyType: string.unpack (get "energyType" r)
   , output: get_intOrNumber "output" r
   }

tableViewHandler :: Handler
tableViewHandler = const identity
