module DataType where

import Prelude hiding (absurd)
import Data.Char.Unicode (isUpper)
import Data.Either (note)
import Data.Function (on)
import Data.List (fromFoldable) as L
import Data.List (List(..), (:), concat)
import Data.Map (Map, lookup)
import Data.Map (fromFoldable) as M
import Data.Map.Internal (keys)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (charAt)
import Data.Tuple (uncurry)
import Lattice (class Key)
import Util (MayFail, type (×), (×), (=<<<), (≞), absurd, error, fromJust, with)

type TypeName = String
type FieldName = String

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

instance keyCtr :: Key Ctr where
   checkConsistent msg c cs = void $ do
      d <- dataTypeFor c
      d' <- dataTypeFor cs
      with (msg <> show c <> " is not a constructor of " <> show d') (d ≞ d')

data DataType' a = DataType TypeName (Map Ctr a)
type DataType = DataType' CtrSig
type CtrSig = Int

typeName :: DataType -> TypeName
typeName (DataType name _) = name

instance eqDataType :: Eq (DataType' Int) where
   eq = eq `on` typeName

instance showDataType :: Show (DataType' Int) where
   show = typeName

dataType :: TypeName -> Array (Ctr × CtrSig) -> DataType
dataType name = map (uncurry (×)) >>> M.fromFoldable >>> DataType name

ctrToDataType :: Map Ctr DataType
ctrToDataType = M.fromFoldable (concat (dataTypes <#> (\d -> ctrs d <#> (_ × d))))

class DataTypeFor a where
   dataTypeFor :: a -> MayFail DataType

instance dataTypeForCtr :: DataTypeFor Ctr where
   dataTypeFor c = note ("Unknown constructor " <> show c) (lookup c ctrToDataType)

instance dataTypeForListCtr :: DataTypeFor (List Ctr) where
   dataTypeFor Nil     = error absurd
   dataTypeFor (c : _) = dataTypeFor c

ctrs :: DataType -> List Ctr
ctrs (DataType _ sigs) = keys sigs

arity :: Ctr -> MayFail Int
arity c = do
   DataType _ sigs <- dataTypeFor c
   note absurd (lookup c sigs)

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = void $
   with ("Checking arity of " <> show c) (arity c `(=<<<) (≞)` pure n)

-- Used internally by primitives, desugaring or rendering layer.
cBarChart   = Ctr "BarChart"  :: Ctr -- Plot
cLineChart  = Ctr "LineChart" :: Ctr
cLinePlot   = Ctr "LinePlot"  :: Ctr
cFalse      = Ctr "False"     :: Ctr -- Bool
cTrue       = Ctr "True"      :: Ctr
cNil        = Ctr "Nil"       :: Ctr -- List
cCons       = Ctr ":"         :: Ctr
cPair       = Ctr "Pair"      :: Ctr -- Pair

dataTypes :: List DataType
dataTypes = L.fromFoldable [
   -- Core
   dataType "Bool" [
      cTrue × 0,
      cFalse × 0
   ],
   dataType "List" [
      cNil × 0,
      cCons × 2 -- any × List<any>
   ],
   dataType "Option" [
      Ctr "None" × 0,
      Ctr "Some" × 1 -- any
   ],
   dataType "Ordering" [
      Ctr "GT" × 0,
      Ctr "LT" × 0,
      Ctr "EQ" × 0
   ],
   dataType "Pair" [
      Ctr "Pair" × 2 -- any × any
   ],
   dataType "Tree" [
      Ctr "Empty" × 0,
      Ctr "NonEmpty" × 3 -- Tree<any> × any × Tree<any>
   ],
   -- Graphics
   dataType "Point" [
      Ctr "Point" × 2 -- Float × Float
   ],

   dataType "Orient" [  -- iso to Bool
      Ctr "Horiz" × 0,
      Ctr "Vert"  × 0
   ],

   dataType "Plot" [
      cBarChart × 2,   -- Record<caption: Str, List<Record<x: Str, y: Float>>>
      cLineChart × 2,  -- Record<caption: Str, List<LinePlot>>
      cLinePlot × 2    -- Record<name: Str, List<Record<x: Float, y: Float>>>
   ],

   dataType "GraphicsElement" [
      Ctr "Circle" × 4,       -- Float (x), Float (y), Float (radius), Str (fill),
      Ctr "Group" × 1,        -- List<GraphicsElement>,
      Ctr "Line" × 4,         -- Float (p1), Float (p2), Str (stroke), Float (strokeWidth),
      Ctr "Polyline" × 3,     -- List<Point> (points), Str (stroke), Float (strokeWidth)
      Ctr "Polymarkers" × 2,  -- List<Point> (points), List<GraphicsElement> (markers),
      Ctr "Rect" × 5,         -- Float (x), Float (y), Float (width), Float (height), Str (fill)
      -- these are SVG text-anchor and alignment-baseline properties
      Ctr "Text" × 5,         -- Float (x), Float (y), Str (str), Str (anchor), Str(baseline)
      -- margin is in *parent* reference frame; scaling applies to translated coordinates
      Ctr "Viewport" × 9      -- Float (x), Float (y), Float (width), Float (height), Str (fill),
                              -- Float (margin), Transform (scale), Transform (translate), GraphicsElement (g)
   ],

   dataType "Transform" [
      Ctr "Scale" × 2, -- Float (x), Float (y)
      Ctr "Translate" × 2 -- Float (x), Float (y)
   ],

   dataType "Marker" [
      Ctr "Arrowhead" × 0
   ]
]
