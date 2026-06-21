module DataType where

import Prelude hiding (absurd)

import Bind (Var)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.CodePoint.Unicode (isUpper)
import Data.Foldable (any)
import Data.Function (on)
import Data.List (List, concat, (:))
import Data.List as List
import Data.List (filter, fromFoldable) as L
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (snd)
import Data.Set (Set)
import Data.Set (map, fromFoldable, toUnfoldable) as S
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (charAt)
import Data.Tuple (uncurry)
import DefiniteAssignment (ClassCtx)
import Dict (Dict, fromFoldable)
import Dict as M
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
isCtrName str = let c = definitely' $ charAt 0 str in isUpper (codePointFromChar c) || c == '_'

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
dataType name = map (uncurry (×)) >>> fromFoldable >>> DataType name

ctrToDataType :: Dict DataType
ctrToDataType =
   dataTypes <#> (\d -> ctrs d # S.toUnfoldable <#> (_ × d)) # concat # fromFoldable

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

-- ====================================================================
-- Parallel Λ-derived implementations (work in progress migration off
-- the static bootstrap above). Each function takes a ClassCtx and
-- returns Nothing on miss so callers can decide how to fail.
-- ====================================================================

-- Walk up the base chain to find the topmost ancestor (self if no base).
rootClass :: ClassCtx -> Ctr -> Ctr
rootClass λ c = case Map.lookup c λ of
   Just (Just b × _) -> rootClass λ b
   _ -> c

-- A class is a concrete ctr iff it has a base, or it has no base and no children.
isCtr :: ClassCtx -> Ctr -> Boolean
isCtr λ c = case Map.lookup c λ of
   Nothing -> false
   Just (Just _ × _) -> true
   Just (Nothing × _) -> not (any (\(_ × (mb × _)) -> mb == Just c) (Map.toUnfoldable λ :: List _))

dataTypeFromClassCtx :: ClassCtx -> Ctr -> Maybe DataType
dataTypeFromClassCtx λ c
   | not (isCtr λ c) = Nothing
   | otherwise =
        let
           r = rootClass λ c
           siblings = Map.toUnfoldable λ # L.filter (\(c' × _) -> isCtr λ c' && rootClass λ c' == r)
        in
           Just (DataType r (fromFoldable (sigOf <$> siblings)))
        where
        sigOf (c' × (mb × xs)) =
           -- Arity = inherited + own (one level of inheritance suffices for current bootstrap).
           let
              inherited = maybe 0 (\b -> maybe 0 (List.length <<< snd) (Map.lookup b λ)) mb
           in
              c' × (inherited + List.length xs)

arityFromClassCtx :: ClassCtx -> Ctr -> Maybe Int
arityFromClassCtx λ c = do
   DataType _ sigs <- dataTypeFromClassCtx λ c
   lookup c sigs

-- Used internally by primitives, desugaring or rendering layer.
cDefault = "Default" :: Ctr -- Orientation
cRotated = "Rotated" :: Ctr
cBarChart = "BarChart" :: Ctr -- View
cLineChart = "LineChart" :: Ctr
cLinePlot = "LinePlot" :: Ctr
cMultiView = "MultiView" :: Ctr
cScatterPlot = "ScatterPlot" :: Ctr
cParagraph = "Paragraph" :: Ctr
cFalse = "False" :: Ctr -- Bool
cTrue = "True" :: Ctr
cNil = "Nil" :: Ctr -- List
cCons = "Cons" :: Ctr
cPair = "Pair" :: Ctr -- Pair
cNothing = "Nothing" :: Ctr -- Maybe
cJust = "Just" :: Ctr
cNone = "None" :: Ctr -- NoneType
cNoArgs = "__NoArgs" :: Ctr -- internal: zero-arg fn signature/call
cText = "Text" :: Ctr
cLink = "Link" :: Ctr
-- Field names used internally by rendering layer.
f_caption = "caption" :: FieldName
f_colour = "c" :: FieldName
f_height = "height" :: FieldName
f_labels = "labels" :: FieldName
f_legend = "legend" :: FieldName
f_name = "name" :: FieldName
f_plots = "plots" :: FieldName
f_points = "points" :: FieldName
f_segments = "segments" :: FieldName
f_size = "size" :: FieldName
f_stackedBars = "stackedBars" :: FieldName
f_tickLabels = "tickLabels" :: FieldName
f_width = "width" :: FieldName
f_x = "x" :: FieldName
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
   , dataType "Maybe"
        [ cNothing × 0
        , cJust × 1 -- any
        ]
   , dataType "NoneType"
        [ cNone × 0
        ]
   , dataType "__NoArgs"
        [ cNoArgs × 0
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
        , cMultiView × 1
        , cParagraph × 1
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
        , "String" × 5 -- Float (x), Float (y), Str (str), Str (anchor), Str(baseline)
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
   , dataType "ParaFragment"
        [ cText × 1 -- Str (str)
        , cLink × 2 --  Val v, Str (str)
        ]
   ]
