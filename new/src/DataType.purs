module DataType where

import Prelude hiding (absurd)
import Data.Char.Unicode (isUpper)
import Data.Either (note)
import Data.Foldable (class Foldable)
import Data.List (fromFoldable) as L
import Data.List (List, concat, length)
import Data.Map (Map, fromFoldable, lookup)
import Data.Map.Internal (keys)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (charAt)
import Util (MayFail, type (×), (×), absurd, fromJust)

type TypeName = String

-- A Ctr is a purely syntactic notion. There may be no constructor with such a name.
newtype Ctr = Ctr String
derive instance newtypeCtr :: Newtype Ctr _
derive instance eqCtr :: Eq Ctr
derive instance ordCtr :: Ord Ctr

-- Distinguish constructors from identifiers syntactically, a la Haskell. In particular this is useful
-- for distinguishing pattern variables from nullary constructors when parsing patterns.
isCtrName ∷ String → Boolean
isCtrName str = isUpper $ fromJust absurd $ charAt 0 str

instance showCtr :: Show Ctr where
   -- assume binary infix if not constructor name
   show c = if isCtrName str then str else "(" <> str <> ")"
      where str = unwrap c

data DataType' a = DataType TypeName (Map Ctr a)
type DataType = DataType' CtrSig
type CtrSig = List TypeName

typeName :: DataType -> TypeName
typeName (DataType name _) = name

ctr :: forall f . Foldable f => Ctr -> f TypeName -> Ctr × CtrSig
ctr c = L.fromFoldable >>> (×) c

dataType :: forall f . Foldable f => TypeName -> f (Ctr × CtrSig) -> DataType
dataType name = fromFoldable >>> DataType name

ctrToDataType :: Map Ctr DataType
ctrToDataType = fromFoldable $
   concat $ dataTypes <#> (\d@(DataType _ sigs) -> keys sigs <#> \c -> c × d)

dataTypeFor :: Ctr -> MayFail DataType
dataTypeFor c = note ("Unknown constructor " <> show c) $ lookup c ctrToDataType

arity :: Ctr -> MayFail Int
arity c = do
   DataType _ sigs <- dataTypeFor c
   length <$> note absurd (lookup c sigs)

cFalse   = Ctr "False"  :: Ctr -- Bool
cTrue    = Ctr "True"   :: Ctr
cNil     = Ctr "Nil"    :: Ctr -- List
cCons    = Ctr ":"      :: Ctr
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
