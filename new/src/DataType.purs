module DataType where

import Prelude
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List (fromFoldable) as L
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))

data DataType = DataType String (Map String Ctr)
data Ctr = Ctr String (List String)

ctr :: forall f . Foldable f => String -> f String -> Tuple String Ctr
ctr c = L.fromFoldable >>> Ctr c >>> Tuple c

dataType :: forall f . Foldable f => String -> f (Tuple String Ctr) -> DataType
dataType name = fromFoldable >>> DataType name

dataTypes :: List DataType
dataTypes = L.fromFoldable [
   dataType "Bool" [
      ctr "True" [],
      ctr "False" []
   ],
   dataType "List" [
      ctr "Nil" [],
      ctr "Cons" ["head", "tail"]
   ]
]
