module DataType where

import Prelude hiding (absurd)
import Data.Char.Unicode (isUpper)
import Data.Either (note)
import Data.Foldable (class Foldable, foldr)
import Data.List (fromFoldable) as L
import Data.List (List(..), concat, length, (:), findIndex, alterAt)
import Data.Map (Map, lookup)
import Data.Map (fromFoldable, toUnfoldable) as M
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
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

cFalse      = Ctr "False"     :: Ctr -- Bool
cTrue       = Ctr "True"      :: Ctr
cNil        = Ctr "Nil"       :: Ctr -- List
cCons       = Ctr ":"         :: Ctr
cNone       = Ctr "None"      :: Ctr -- Option
cSome       = Ctr "Some"      :: Ctr
cGT         = Ctr "GT"        :: Ctr -- Ordering
cLT         = Ctr "LT"        :: Ctr
cEQ         = Ctr "EQ"        :: Ctr
cPair       = Ctr "Pair"      :: Ctr -- Pair
cEmpty      = Ctr "Empty"     :: Ctr -- Tree
cNonEmpty   = Ctr "NonEmpty"  :: Ctr

ctrToDataTypeStr :: Ctr -> String
ctrToDataTypeStr ctr'
   = let m = ctrDataTypeMap
   in fromJust "" $ lookup ctr' m

dataTypeStrToCtrs :: String -> List Ctr
dataTypeStrToCtrs dt
   = let m = flipMap ctrDataTypeMap
     in fromJust "" $ lookup dt m

flipMap :: forall k v. Eq v => Ord v => Map k v -> Map v (List k)
flipMap m
   = let f   = \(k × v) v_to_ks
                  -> updateAt (\(v' × _) -> v == v')
                              (\(_ × ks) -> v × (k:ks))
                              (v × (k:Nil)) v_to_ks

     in M.fromFoldable $ foldr f Nil (M.toUnfoldable m :: List (k × v))

updateAt :: forall a. (a -> Boolean) -> (a -> a) -> a -> List a -> List a
updateAt predicate f default xs
   = case findIndex predicate xs of
      Just i -> fromJust "" $ alterAt i (\x -> Just (f x)) xs
      Nothing -> (default:xs)

ctrDataTypeMap :: Map Ctr String
ctrDataTypeMap = M.fromFoldable [
   cFalse × "Bool",
   cTrue × "Bool",
   cNil × "List",
   cCons × "List",
   cNone × "Option",
   cSome × "Option",
   cGT × "Ordering",
   cLT × "Ordering",
   cEQ × "Ordering",
   cPair × "Pair",
   cEmpty × "Tree",
   cNonEmpty × "Tree"]

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
   dataType "Option" [
      ctr cNone [],
      ctr cSome ["x"]
   ],
   dataType "Ordering" [
      ctr cGT [],
      ctr cLT [],
      ctr cEQ []
   ],
   dataType "Pair" [
      ctr cPair ["fst", "snd"]
   ],
   dataType "Tree" [
      ctr cEmpty [],
      ctr cNonEmpty ["left", "x", "right"]
   ]
]
