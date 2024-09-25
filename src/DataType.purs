module DataType where

import Prelude hiding (absurd)

import Bind (Var)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.CodePoint.Unicode (isUpper)
import Data.Function (on)
import Data.List (List, concat, (:))
import Data.List (fromFoldable) as L
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set (map, fromFoldable, toUnfoldable) as S
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (charAt)
import Data.Tuple (uncurry)
import Dict (Dict)
import Dict (fromFoldable) as O
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Util (type (×), absurd, definitely', error, orElse, withMsg, (=<<<), (×), (≞))
import Util.Map (keys, lookup)

type TypeName = String
type FieldName = String
type Ctr = String -- newtype would require more general Dict keys

-- Distinguish constructors from identifiers syntactically, a la Haskell. In particular this is useful
-- for distinguishing pattern variables from nullary constructors when parsing patterns.
isCtrName ∷ Var → Boolean
isCtrName str = isUpper $ codePointFromChar $ definitely' $ charAt 0 str

isCtrOp :: String -> Boolean
isCtrOp str = ':' == (definitely' $ charAt 0 str)

showCtr :: Ctr -> String
showCtr c
   | isCtrName c = c
   | isCtrOp c = "(" <> c <> ")"
   | otherwise = error absurd

data DataType = DataType TypeName (Dict CtrSig)
type CtrSig = Int

typeName :: DataType -> TypeName
typeName (DataType name _) = name

instance Eq DataType where
   eq = eq `on` typeName

instance Show DataType where
   show = typeName

dataType :: TypeName -> Array (Ctr × CtrSig) -> DataType
dataType name = map (uncurry (×)) >>> O.fromFoldable >>> wrap >>> DataType name

ctrToDataType :: Dict DataType
ctrToDataType =
   dataTypes <#> (\d -> ctrs d # S.toUnfoldable <#> (_ × d)) # concat # O.fromFoldable # wrap

class DataTypeFor a where
   dataTypeFor :: forall m. MonadThrow Error m => a -> m DataType

instance DataTypeFor Ctr where
   dataTypeFor c = lookup c ctrToDataType # orElse ("Unknown constructor " <> showCtr c)

instance DataTypeFor (Set Ctr) where
   dataTypeFor cs = unsafePartial $ case S.toUnfoldable cs of c : _ -> dataTypeFor c

-- Sets must be non-empty, but this is a more convenient signature.
consistentWith :: forall m. MonadError Error m => Set Ctr -> Set Ctr -> m Unit
consistentWith cs cs' = void do
   d <- dataTypeFor cs'
   d' <- dataTypeFor cs'
   withMsg ("constructors of " <> show d' <> " do not include " <> (show (S.map showCtr cs))) (d ≞ d')

ctrs :: DataType -> Set Ctr
ctrs (DataType _ sigs) = keys sigs # S.fromFoldable

arity :: forall m. MonadThrow Error m => Ctr -> m Int
arity c = do
   DataType _ sigs <- dataTypeFor c
   lookup c sigs # orElse absurd

checkArity :: forall m. MonadError Error m => Ctr -> Int -> m Unit
checkArity c n = void $
   withMsg ("Checking arity of " <> showCtr c) (arity c `(=<<<) (≞)` pure n)

-- Used internally by primitives, desugaring or rendering layer.
cDefault = "Default" :: Ctr -- Orientation
cRotated = "Rotated" :: Ctr
cBarChart = "BarChart" :: Ctr -- View
cLineChart = "LineChart" :: Ctr
cLinePlot = "LinePlot" :: Ctr
cMultiView = "MultiView" :: Ctr
cScatterPlot = "ScatterPlot" :: Ctr
cLinkedText = "LinkedText" :: Ctr
cFalse = "False" :: Ctr -- Bool
cTrue = "True" :: Ctr
cNil = "Nil" :: Ctr -- List
cCons = ":" :: Ctr
cPair = "Pair" :: Ctr -- Pair
cNone = "None" :: Ctr -- Option
cSome = "Some" :: Ctr

-- Field names used internally by rendering layer.
f_bars = "bars" :: FieldName
f_caption = "caption" :: FieldName
f_colour = "c" :: FieldName
f_name = "name" :: FieldName
f_plots = "plots" :: FieldName
f_points = "points" :: FieldName
f_size = "size" :: FieldName
f_stackedBars = "stackedBars" :: FieldName
f_tickLabels = "tickLabels" :: FieldName
f_x = "x" :: FieldName
f_xlabel = "xlabel" :: FieldName
f_ylabel = "ylabel" :: FieldName
f_y = "y" :: FieldName
f_z = "z" :: FieldName

dataTypes :: List DataType
dataTypes = L.fromFoldable
   [
     -- Core
     dataType "Bool"
        [ cTrue × 0
        , cFalse × 0
        ]
   , dataType "InfNum"
        [ "FNum" × 1
        , "Infty" × 0
        ]
   , dataType "List"
        [ cNil × 0
        , cCons × 2 -- any × List any
        ]
   , dataType "Option"
        [ cNone × 0
        , cSome × 1 -- any
        ]
   , dataType "Ordering"
        [ "GT" × 0
        , "LT" × 0
        , "EQ" × 0
        ]
   , dataType "Pair"
        [ "Pair" × 2 -- any × any
        ]
   , dataType "Tree"
        [ "Empty" × 0
        , "NonEmpty" × 3 -- Tree any × any × Tree any
        ]
   -- View stuff
   , dataType "LinePlot"
        [ cLinePlot × 1
        ]
   , dataType "Orientation"
        [ cDefault × 0
        , cRotated × 0
        ]
   , dataType "View"
        [ cBarChart × 1
        , cLineChart × 1
        , cLinkedText × 1
        , cMultiView × 1
        , cScatterPlot × 1
        ]
   ,
     -- Legacy graphics stuff
     dataType "Point"
        [ "Point" × 2 -- Float × Float
        ]
   , dataType "Orient"
        [ -- iso to Bool
          "Horiz" × 0
        , "Vert" × 0
        ]
   , dataType "GraphicsElement"
        [ "Circle" × 4 -- Float (x), Float (y), Float (radius), Str (fill)
        , "Group" × 1 -- List GraphicsElement
        , "Line" × 4 -- Float (p1), Float (p2), Str (stroke), Float (strokeWidth)
        , "Polyline" × 3 -- List Point (points), Str (stroke), Float (strokeWidth)
        , "Polymarkers" × 2 -- List Point (points), List GraphicsElement (markers)
        , "Rect" × 5 -- Float (x), Float (y), Float (width), Float (height), Str (fill)
        -- SVG text-anchor and alignment-baseline properties
        , "Text" × 5 -- Float (x), Float (y), Str (str), Str (anchor), Str(baseline)
        -- margin is in *parent* reference frame; scaling applies to translated coordinates
        , "Viewport" × 9 -- Float (x), Float (y), Float (width), Float (height), Str (fill),
        -- Float (margin), Transform (scale), Transform (translate), GraphicsElement (g)
        ]
   , dataType "Transform"
        [ "Scale" × 2 -- Float (x), Float (y)
        , "Translate" × 2 -- Float (x), Float (y)
        ]
   , dataType "Marker"
        [ "Arrowhead" × 0
        ]
   ]
