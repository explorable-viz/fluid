module DataType where

import Prelude hiding (absurd)

import Bind (Name, Var, dottedName, qual)
import Control.Monad.Error.Class (class MonadError)
import Data.CodePoint.Unicode (isUpper)
import Data.Foldable (any, for_)
import Data.Function (on)
import Data.List (List(..), elemIndex, (:))
import Data.List as List
import Data.List (filter) as L
import Data.List.NonEmpty (NonEmptyList(..)) as NE
import Data.NonEmpty ((:|))
import Data.Map as Map
import Data.Array (last) as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Set (Set)
import Data.Set (fromFoldable, map, toUnfoldable) as S
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (charAt)
import DefiniteAssignment (ClassEntry, Cxt, classFor, classesOf, fields)
import Dict (Dict, fromFoldable)
import Effect.Exception (Error)
import Util (absurd, definitely', error, throw, withMsg, (×))
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
   | isCtrName (simpleName c) = simpleName c
   | isCtrOp (simpleName c) = "(" <> simpleName c <> ")"
   | otherwise = error absurd

data DataType = DataType TypeName (Dict CtrSig)
type CtrSig = Int

typeName :: DataType -> TypeName
typeName (DataType name _) = name

instance Eq DataType where
   eq = eq `on` typeName

instance Show DataType where
   show = typeName

ctrs :: DataType -> Set Ctr
ctrs (DataType _ sigs) = keys sigs # S.fromFoldable

consistentWith :: forall m. MonadError Error m => Cxt -> Set Ctr -> Set Ctr -> m Unit
consistentWith γ cs cs' = case S.toUnfoldable cs' :: List Ctr of
   Nil -> pure unit
   c : _ -> case dataType γ c of
      Nothing -> throw $ "Unknown dataclass: " <> showCtr c
      Just d -> withMsg ("dataclasses of " <> show d <> " do not include " <> show (S.map showCtr cs))
         $ for_ (S.toUnfoldable cs :: List Ctr) \c'' -> case dataType γ c'' of
              Just d'' | d'' == d -> pure unit
              _ -> throw "mismatch"

checkArity :: forall m. MonadError Error m => Cxt -> Ctr -> Int -> m Unit
checkArity γ c n = case arity γ c of
   Just n' | n' == n -> pure unit
   Just n' -> throw $ showCtr c <> " arity " <> show n' <> "; got " <> show n
   Nothing -> throw $ "Unknown dataclass: " <> showCtr c

-- A class entry's base, as a fully-qualified name (entries store its simple name).
baseFqn :: ClassEntry -> Maybe Ctr
baseFqn ce = (dottedName <<< qual ce.mod) <$> ce.base

rootClass :: Map.Map Var ClassEntry -> Ctr -> Ctr
rootClass λ c = case Map.lookup c λ of
   Just ce | Just b <- baseFqn ce -> rootClass λ b
   _ -> c

-- Concrete iff a leaf.
isCtr :: Map.Map Var ClassEntry -> Ctr -> Boolean
isCtr λ c = Map.member c λ && not (any (\(_ × ce) -> baseFqn ce == Just c) (Map.toUnfoldable λ :: List _))

dataType :: Cxt -> Ctr -> Maybe DataType
dataType γ c =
   if isCtr λ c then Just (DataType r (fromFoldable (sigOf <$> siblings)))
   else Nothing
   where
   λ = classesOf γ
   r = rootClass λ c
   siblings = Map.toUnfoldable λ # L.filter (\(c' × _) -> isCtr λ c' && rootClass λ c' == r)
   sigOf (c' × ce) = c' × List.length (fields ce)

arity :: Cxt -> Ctr -> Maybe Int
arity γ c = do
   DataType _ sigs <- dataType γ c
   lookup c sigs

fieldIndex :: Cxt -> Name -> FieldName -> Maybe Int
fieldIndex γ c field = do
   ce <- classFor γ (dottedName c)
   elemIndex field (fields ce)

-- Module paths for the builtin/library constructors (hard-coded for now).
lib_builtins :: Var -> Name
lib_builtins = qual (NE.NonEmptyList ("lib" :| "builtins" : Nil))

lib_view :: Var -> Name
lib_view = qual (NE.NonEmptyList ("lib" :| "view" : Nil))

-- Last (simple) segment of a possibly-qualified constructor name.
simpleName :: Ctr -> String
simpleName c = fromMaybe c (A.last (split (Pattern ".") c))

-- Used internally by primitives, desugaring or rendering layer.
cDefault = lib_view "Default" :: Name -- Orientation
cRotated = lib_view "Rotated" :: Name
cBarChart = lib_view "BarChart" :: Name -- View
cLineChart = lib_view "LineChart" :: Name
cLinePlot = lib_view "LinePlot" :: Name
cMultiView = lib_view "MultiView" :: Name
cScatterPlot = lib_view "ScatterPlot" :: Name
cParagraph = lib_view "Paragraph" :: Name
cFalse = lib_builtins "False" :: Name -- Bool
cTrue = lib_builtins "True" :: Name
cNil = lib_builtins "Nil" :: Name -- List
cCons = lib_builtins "Cons" :: Name
cPair = lib_builtins "Pair" :: Name -- Pair
cNothing = lib_builtins "Nothing" :: Name -- Maybe
cJust = lib_builtins "Just" :: Name
cNone = lib_builtins "None" :: Name -- NoneType
cNoArgs = lib_builtins "__NoArgs" :: Name -- internal: zero-arg fn signature/call
cText = lib_view "Text" :: Name
cLink = lib_view "Link" :: Name
-- Field names used internally by rendering layer.
f_caption = "caption" :: FieldName
f_fragments = "fragments" :: FieldName
f_label = "label" :: FieldName
f_text = "text" :: FieldName
f_value = "value" :: FieldName
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
f_views = "views" :: FieldName
f_width = "width" :: FieldName
f_x = "x" :: FieldName
f_y = "y" :: FieldName
f_z = "z" :: FieldName

