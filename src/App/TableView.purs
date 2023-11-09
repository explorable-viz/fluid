module App.TableView where

import Prelude

import App.Util (Handler, Renderer, get_intOrNumber)
import Dict (Dict, get)
import Dict as Dict
import Lattice (𝔹)
import Primitive (int, string, number)
import Util (type (×), (×))
import Val (Val)

-- For each user-level datatype of interest, a representation containing appropriate implementation types.
-- Record types are hardcoded to a single specific example for now.
newtype EnergyRecord = EnergyRecord { year :: Int × 𝔹, country :: String × 𝔹, energyType :: String × 𝔹, output :: Number × 𝔹 }
newtype EnergyTable = EnergyTable { title :: String, table :: Array EnergyRecord }

newtype WaterRecord = WaterRecord { country :: String × 𝔹, cities :: Int × 𝔹, farms :: Int × 𝔹, industry :: Int × 𝔹, energy :: Int × 𝔹, popMil :: Int × 𝔹 }
newtype WaterTable = WaterTable { title :: String, table :: Array WaterRecord }

newtype Table r = Table { title :: String, table :: Array r}

foreign import drawTable :: Renderer (Table (Dict (Val 𝔹)))

class MakeRecord a where
   fromRec :: Dict (Val 𝔹) -> a
   toDict :: a -> Dict (Val 𝔹)
instance MakeRecord WaterRecord where
   fromRec r = 
      WaterRecord { country: string.unpack (get "country" r)
      , cities: int.unpack (get "cities" r)
      , farms: int.unpack (get "farms" r)
      , industry: int.unpack (get "industry" r)
      , energy: int.unpack (get "energy" r)
      , popMil: int.unpack (get "popMil" r)
      }
   toDict (WaterRecord { country, cities, farms, industry, energy, popMil}) =
      Dict.fromFoldable (["country" × string.pack country, "cities" × (int.pack cities), "farms" × int.pack farms, "industry" × int.pack industry, "energy" × int.pack energy, "popMil" × int.pack popMil])
instance MakeRecord EnergyRecord where
   fromRec r =
      EnergyRecord { year: int.unpack (get "year" r)
      , country: string.unpack (get "country" r)
      , energyType: string.unpack (get "energyType" r)
      , output: get_intOrNumber "output" r
      }
   toDict (EnergyRecord {year, country, energyType, output}) = 
      Dict.fromFoldable (["year" × int.pack year, "country" × string.pack country, "energyType" × string.pack energyType, "output" × number.pack output])

tableViewHandler :: Handler
tableViewHandler = const identity
