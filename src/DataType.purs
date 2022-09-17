module DataType where

import Prelude hiding (absurd)
import Data.Array (head)
import Data.CodePoint.Unicode (isUpper)
import Data.Either (note)
import Data.Function (on)
import Data.List (fromFoldable) as L
import Data.List (List, concat)
import Data.Map (Map, lookup, keys)
import Data.Map (fromFoldable) as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set, toUnfoldable)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (charAt)
import Data.Tuple (uncurry)
import Bindings (Var)
import Util (MayFail, type (×), (×), (=<<<), (≞), absurd, error, definitely', with)

type TypeName = String
type FieldName = String
type Ctr = String -- newtype would be nicer but JS maps are also nice

-- Distinguish constructors from identifiers syntactically, a la Haskell. In particular this is useful
-- for distinguishing pattern variables from nullary constructors when parsing patterns.
isCtrName ∷ Var → Boolean
isCtrName str = isUpper $ codePointFromChar $ definitely' $ charAt 0 str

isCtrOp :: String -> Boolean
isCtrOp str = ':' == (definitely' $ charAt 0 str)

showCtr :: Ctr -> String
showCtr c | isCtrName c = c
          | isCtrOp c   = "(" <> c <> ")"
          | otherwise   = error absurd

-- Check that two non-empty sets of identifiers (each assumed to contain constructors of a unique datatype),
-- contain constructors from the same datatype.
consistentCtrs :: Array Ctr -> Array Ctr -> MayFail Unit
consistentCtrs cs cs' = void $ do
   d <- dataTypeFor cs
   d' <- dataTypeFor cs'
   with ("constructors of " <> show d' <> " do not include " <> (show (cs <#> showCtr))) (d ≞ d')

data DataType' a = DataType TypeName (Map Ctr a)
type DataType = DataType' CtrSig
type CtrSig = Int

typeName :: DataType -> TypeName
typeName (DataType name _) = name

instance Eq (DataType' Int) where
   eq = eq `on` typeName

instance Show (DataType' Int) where
   show = typeName

dataType :: TypeName -> Array (Ctr × CtrSig) -> DataType
dataType name = map (uncurry (×)) >>> M.fromFoldable >>> DataType name

ctrToDataType :: Map Ctr DataType
ctrToDataType =
   dataTypes <#> (\d -> ctrs d # toUnfoldable <#> (_ × d)) # concat # M.fromFoldable

class DataTypeFor a where
   dataTypeFor :: a -> MayFail DataType

instance DataTypeFor Ctr where
   dataTypeFor c = note ("Unknown constructor " <> showCtr c) (lookup c ctrToDataType)

instance DataTypeFor (Array Ctr) where
   dataTypeFor cs = case head cs of
      Nothing  -> error absurd
      Just c   -> dataTypeFor c

ctrs :: DataType -> Set Ctr
ctrs (DataType _ sigs) = keys sigs

arity :: Ctr -> MayFail Int
arity c = do
   DataType _ sigs <- dataTypeFor c
   note absurd (lookup c sigs)

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = void $
   with ("Checking arity of " <> showCtr c) (arity c `(=<<<) (≞)` pure n)

-- Used internally by primitives, desugaring or rendering layer.
cBarChart   = "BarChart"  :: Ctr -- Plot
cLineChart  = "LineChart" :: Ctr
cLinePlot   = "LinePlot"  :: Ctr
cFalse      = "False"     :: Ctr -- Bool
cTrue       = "True"      :: Ctr
cNil        = "Nil"       :: Ctr -- List
cCons       = ":"         :: Ctr
cPair       = "Pair"      :: Ctr -- Pair
cNone       = "None"      :: Ctr -- Option
cSome       = "Some"      :: Ctr

-- Field names used internally by rendering layer.
f_caption   = "caption" :: FieldName
f_data      = "data"    :: FieldName
f_name      = "name"    :: FieldName
f_plots     = "plots"   :: FieldName
f_x         = "x"       :: FieldName
f_y         = "y"       :: FieldName

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
      cNone × 0,
      cSome × 1 -- any
   ],
   dataType "Ordering" [
      "GT" × 0,
      "LT" × 0,
      "EQ" × 0
   ],
   dataType "Pair" [
      "Pair" × 2 -- any × any
   ],
   dataType "Tree" [
      "Empty" × 0,
      "NonEmpty" × 3 -- Tree<any> × any × Tree<any>
   ],
   -- Graphics
   dataType "Point" [
      "Point" × 2 -- Float × Float
   ],

   dataType "Orient" [  -- iso to Bool
      "Horiz" × 0,
      "Vert"  × 0
   ],

   dataType "Plot" [
      cBarChart × 1,   -- Record<caption: Str, data: List<Record<x: Str, y: Float>>>
      cLineChart × 1,  -- Record<caption: Str, plots: List<LinePlot>>
      cLinePlot × 1    -- Record<name: Str, data: List<Record<x: Float, y: Float>>>
   ],

   dataType "GraphicsElement" [
      "Circle" × 4,       -- Float (x), Float (y), Float (radius), Str (fill),
      "Group" × 1,        -- List<GraphicsElement>,
      "Line" × 4,         -- Float (p1), Float (p2), Str (stroke), Float (strokeWidth),
      "Polyline" × 3,     -- List<Point> (points), Str (stroke), Float (strokeWidth)
      "Polymarkers" × 2,  -- List<Point> (points), List<GraphicsElement> (markers),
      "Rect" × 5,         -- Float (x), Float (y), Float (width), Float (height), Str (fill)
      -- SVG text-anchor and alignment-baseline properties
      "Text" × 5,         -- Float (x), Float (y), Str (str), Str (anchor), Str(baseline)
      -- margin is in *parent* reference frame; scaling applies to translated coordinates
      "Viewport" × 9      -- Float (x), Float (y), Float (width), Float (height), Str (fill),
                          -- Float (margin), Transform (scale), Transform (translate), GraphicsElement (g)
   ],

   dataType "Transform" [
      "Scale" × 2, -- Float (x), Float (y)
      "Translate" × 2 -- Float (x), Float (y)
   ],

   dataType "Marker" [
      "Arrowhead" × 0
   ]
]
