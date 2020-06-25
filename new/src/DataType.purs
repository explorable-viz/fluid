module DataType where

import Prelude
import Data.Foldable (class Foldable)
import Data.List (fromFoldable) as L
import Data.List (List)
import Data.Map (Map, fromFoldable)
import Data.Newtype (class Newtype, unwrap)
import Util (type (×), (×))

newtype Ctr = Ctr String
derive instance newtypeCtr :: Newtype Ctr _
derive instance eqCtr :: Eq Ctr
derive instance ordCtr :: Ord Ctr

instance showCtr :: Show Ctr where
   show = unwrap

data DataType' a = DataType String (Map Ctr a)
type DataType = DataType' CtrSig
data CtrSig = CtrSig Ctr (List String)

ctr :: forall f . Foldable f => Ctr -> f String -> Ctr × CtrSig
ctr c = L.fromFoldable >>> CtrSig c >>> (×) c

dataType :: forall f . Foldable f => String -> f (Ctr × CtrSig) -> DataType
dataType name = fromFoldable >>> DataType name

cFalse   = Ctr "False"  :: Ctr -- Bool
cTrue    = Ctr "True"   :: Ctr
cNil     = Ctr "Nil"    :: Ctr -- List
cCons    = Ctr "Cons"   :: Ctr
cGT      = Ctr "GT"     :: Ctr -- Ordering
cLT      = Ctr "LT"     :: Ctr
cEQ      = Ctr "EQ"     :: Ctr
cPair    = Ctr "Pair"   :: Ctr -- Pair

dataTypes :: List DataType
dataTypes = L.fromFoldable [
   dataType "Bool" [
      ctr cTrue [],
      ctr cFalse []
   ],
   dataType "List" [
      ctr cNil [],
      ctr cCons ["head", "tail"]
   ],
   dataType "Ordering" [
      ctr cGT [],
      ctr cLT [],
      ctr cEQ []
   ],
   dataType "Pair" [
      ctr cPair ["fst", "snd"]
   ]
]
