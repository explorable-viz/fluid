module DataType where

import Prelude hiding (absurd)
import Data.Char.Unicode (isUpper)
import Data.Either (note)
import Data.Foldable (class Foldable)
import Data.Function (on)
import Data.List (fromFoldable) as L
import Data.List (List(..), (:), concat, length)
import Data.Map (Map, lookup)
import Data.Map (fromFoldable) as M
import Data.Map.Internal (keys)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (charAt)
import Data.Tuple (uncurry)
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

data DataType' a = DataType TypeName (Map Ctr a)
type DataType = DataType' CtrSig
type CtrSig = List FieldName -- but see issue #162

typeName :: DataType -> TypeName
typeName (DataType name _) = name

instance eqDataType :: Eq (DataType' (List String)) where
   eq = eq `on` typeName

instance showDataType :: Show (DataType' (List String)) where
   show = typeName

ctr :: forall f . Foldable f => Ctr -> f FieldName -> Ctr × CtrSig
ctr c = L.fromFoldable >>> (×) c

dataType :: forall f . Functor f => Foldable f => TypeName -> f (Ctr × f FieldName) -> DataType
dataType name = map (uncurry ctr) >>> M.fromFoldable >>> DataType name

ctrToDataType :: Map Ctr DataType
ctrToDataType = M.fromFoldable $
   concat $ dataTypes <#> (\d@(DataType _ sigs) -> keys sigs <#> (_ × d))

dataTypeFor :: Ctr -> MayFail DataType
dataTypeFor c = note ("Unknown constructor " <> show c) $ lookup c ctrToDataType

dataTypeForKeys :: List Ctr -> MayFail DataType
dataTypeForKeys cs =
   case cs of
      Nil   -> error absurd
      c' : _ -> dataTypeFor c'

arity :: Ctr -> MayFail Int
arity c = do
   DataType _ sigs <- dataTypeFor c
   length <$> note absurd (lookup c sigs)

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = void $ with ("Checking arity of " <> show c) $
   arity c `(=<<<) (≞)` pure n

checkDataType :: forall a . String -> Ctr -> Map Ctr a -> MayFail Unit
checkDataType msg c κs = void $ do
   d <- dataTypeFor c
   d' <- dataTypeForKeys $ keys κs
   if (d /= d')
   then error "***"
   else with (msg <> show c <> " is not a constructor of " <> show d') $ d ≞ d'

-- Used internally by primitives or desugaring.
cFalse      = Ctr "False"     :: Ctr -- Bool
cTrue       = Ctr "True"      :: Ctr
cNil        = Ctr "Nil"       :: Ctr -- List
cCons       = Ctr ":"         :: Ctr
cPair       = Ctr "Pair"      :: Ctr -- Pair

dataTypes :: List DataType
dataTypes = L.fromFoldable [
   -- Core
   dataType "Bool" [
      cTrue    × [],
      cFalse   × []
   ],
   dataType "List" [
      cNil  × [],
      cCons × [
         "head",  -- any
         "tail"   -- List<any>
      ]
   ],
   dataType "Option" [
      Ctr "None"  × [],
      Ctr "Some"  × [
         "x"   -- any
      ]
   ],
   dataType "Ordering" [
      Ctr "GT" × [],
      Ctr "LT" × [],
      Ctr "EQ" × []
   ],
   dataType "Pair" [
      Ctr "Pair" × [
         "fst",   -- any
         "snd"    -- any
      ]
   ],
   dataType "Tree" [
      Ctr "Empty"    × [],
      Ctr "NonEmpty" × [
         "left",  -- Tree<any>
         "x",     -- any
         "right"  -- Tree<any>
      ]
   ],
   -- Graphics
   dataType "Point" [
      Ctr "Point" × [
         "x",  -- Float
         "y"   -- Float
      ]
   ],

   dataType "Orient" [  -- iso to Bool
      Ctr "Horiz" × [],
      Ctr "Vert"  × []
   ],

   dataType "GraphicsElement" [
      Ctr "Circle" × [
         "x",        -- Float
         "y",        -- Float
         "radius",   -- Float
         "fill"      -- Str
      ],
      Ctr "Group" × [
         "gs"  -- List<GraphicsElement>
      ],
      Ctr "Line" × [
         "p1",          -- Float
         "p2",          -- Float
         "stroke",      -- Str
         "strokeWidth"  -- Float
      ],
      Ctr "Polyline" × [
         "points",      -- List<Point>
         "stroke",      -- Str
         "strokeWidth"  -- Float
      ],
      Ctr "Polymarkers" × [
         "points",   -- List<Point>
         "markers"   -- List<GraphicsElement>
      ],
      Ctr "Rect" × [
         "x",        -- Float
         "y",        -- Float
         "width",    -- Float
         "height",   -- Float
         "fill"      -- Str
      ],
      Ctr "Text" × [
         "x",        -- Float
         "y",        -- Float
         "str",      -- Str
         "anchor",   -- Str (SVG text-anchor)
         "baseline"  -- Str (SVG alignment-baseline)
      ],
      Ctr "Viewport" × [
         "x",           -- Float
         "y",           -- Float
         "width",       -- Float
         "height",      -- Float
         "fill",        -- Str
         "margin",      -- Float (in *parent* reference frame)
         "scale",       -- Transform
         "translate",   -- Transform (scaling applies to translated coordinates)
         "g"            -- GraphicsElement
      ]
   ],

   dataType "Transform" [
      Ctr "Scale" × [
         "x",  -- Float
         "y"   -- Float
      ],
      Ctr "Translate" × [
         "x",  -- Float
         "y"   -- Float
      ]
   ],

   dataType "Marker" [
      Ctr "Arrowhead" × []
   ]
]
