module DataType where

import Prelude hiding (absurd)
import Data.Char.Unicode (isUpper)
import Data.Either (note)
import Data.Foldable (class Foldable)
import Data.List (fromFoldable) as L
import Data.List (List, concat, length)
import Data.Map (Map, lookup)
import Data.Map (fromFoldable) as M
import Data.Map.Internal (keys)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (charAt)
import Util (MayFail, type (×), (×), absurd, error, fromJust)

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

isCtrOp :: String -> Boolean
isCtrOp str = ':' == (fromJust absurd $ charAt 0 str)

instance showCtr :: Show Ctr where
   -- assume binary infix if not constructor name
   show c = show' $ unwrap c where
      show' str | isCtrName str  = str
                | isCtrOp str    = "(" <> str <> ")"
                | otherwise      = error absurd

data DataType' a = DataType TypeName (Map Ctr a)
type DataType = DataType' CtrSig
type CtrSig = List TypeName

typeName :: DataType -> TypeName
typeName (DataType name _) = name

ctr :: forall f . Foldable f => Ctr -> f TypeName -> Ctr × CtrSig
ctr c = L.fromFoldable >>> (×) c

dataType :: forall f . Foldable f => TypeName -> f (Ctr × CtrSig) -> DataType
dataType name = M.fromFoldable >>> DataType name

ctrToDataType :: Map Ctr DataType
ctrToDataType = M.fromFoldable $
   concat $ dataTypes <#> (\d@(DataType _ sigs) -> keys sigs <#> (_ × d))

dataTypeFor :: Ctr -> MayFail DataType
dataTypeFor c = note ("Unknown constructor " <> show c) $ lookup c ctrToDataType

arity :: Ctr -> MayFail Int
arity c = do
   DataType _ sigs <- dataTypeFor c
   length <$> note absurd (lookup c sigs)

-- Some constructor names used internally for primitives, syntactic sugar.
cFalse      = "False"      :: String   -- Bool
cTrue       = "True"       :: String
cNil        = "Nil"        :: String   -- List
cCons       = ":"          :: String
cPair       = "Pair"       :: String   -- Pair

-- Graphics

dataTypes :: List DataType
dataTypes = L.fromFoldable [
   -- Core
   dataType "Bool" [
      ctr (Ctr cTrue) [],
      ctr (Ctr cFalse) []
   ],
   dataType "List" [
      ctr (Ctr cNil) [],
      ctr (Ctr cCons) ["head", "tail"]
   ],
   dataType "Option" [
      ctr (Ctr "None") [],
      ctr (Ctr "Some") ["x"]
   ],
   dataType "Ordering" [
      ctr (Ctr "GT") [],
      ctr (Ctr "LT") [],
      ctr (Ctr "EQ") []
   ],
   dataType "Pair" [
      ctr (Ctr "Pair") ["fst", "snd"]
   ],
   dataType "Tree" [
      ctr (Ctr "Empty") [],
      ctr (Ctr "NonEmpty") ["left", "x", "right"]
   ],
   -- Graphics
   dataType "Point" [
      ctr (Ctr "Point") []
   ],

   dataType "Orient" [
      ctr (Ctr "Horiz") [],
      ctr (Ctr "Vert") []
   ]
]

-- initDataType(GraphicsElement, [Circle, Group, Line, Polyline, Polymarkers, Rect, Text, Viewport])
-- initDataType(Transform, [Scale, Translate])
-- initDataType(Marker, [Arrowhead])
