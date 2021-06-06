module DataType where

import Prelude hiding (absurd)
import Data.Char.Unicode (isUpper)
import Data.Either (note)
import Data.Foldable (class Foldable, length)
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

ctr :: forall f . Foldable f => Ctr -> f FieldName -> Ctr × CtrSig
ctr c fs = c × length fs

dataType :: forall f . Functor f => Foldable f => TypeName -> f (Ctr × f FieldName) -> DataType
dataType name = map (uncurry ctr) >>> M.fromFoldable >>> DataType name

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
         "",  -- any
         ""   -- List<any>
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

   dataType "Plot" [
      Ctr "BarChart" × [
         "dummy"        -- Record<>
      ]
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
