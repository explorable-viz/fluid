module DataType where

import Prelude hiding (absurd)

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.CodePoint.Unicode (isUpper)
import Data.Foldable (any, for_)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.List as List
import Data.List (filter) as L
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (snd)
import Data.Set (Set)
import Data.Set (empty, insert, map, member, fromFoldable, toUnfoldable) as S
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (charAt)
import DefiniteAssignment (ClassCtx)
import Dict (Dict, fromFoldable)
import Effect.Exception (Error)
import Util (type (×), absurd, definitely', error, throw, withMsg, (×))
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

ctrs :: DataType -> Set Ctr
ctrs (DataType _ sigs) = keys sigs # S.fromFoldable

consistentWith :: forall m. MonadError Error m => ClassCtx -> Set Ctr -> Set Ctr -> m Unit
consistentWith λ cs cs' = case S.toUnfoldable cs' :: List Ctr of
   Nil -> pure unit
   c : _ -> case dataType λ c of
      Nothing -> throw $ "Unknown constructor: " <> showCtr c
      Just d -> withMsg ("constructors of " <> show d <> " do not include " <> show (S.map showCtr cs))
         $ for_ (S.toUnfoldable cs :: List Ctr) \c'' -> case dataType λ c'' of
              Just d'' | d'' == d -> pure unit
              _ -> throw "mismatch"

checkArity :: forall m. MonadError Error m => ClassCtx -> Ctr -> Int -> m Unit
checkArity λ c n = case arity λ c of
   Just n' | n' == n -> pure unit
   Just n' -> throw $ showCtr c <> " arity " <> show n' <> "; got " <> show n
   Nothing -> throw $ "Unknown constructor: " <> showCtr c

-- Assumes Λ acyclic.
rootClass :: ClassCtx -> Ctr -> Ctr
rootClass λ c = case Map.lookup c λ of
   Just (Just b × _) -> rootClass λ b
   _ -> c

-- Concrete iff a leaf.
isCtr :: ClassCtx -> Ctr -> Boolean
isCtr λ c = Map.member c λ && not (any (\(_ × (mb × _)) -> mb == Just c) (Map.toUnfoldable λ :: List _))

dataType :: ClassCtx -> Ctr -> Maybe DataType
dataType λ c =
   if isCtr λ c then Just (DataType r (fromFoldable (sigOf <$> siblings)))
   else Nothing
   where
   r = rootClass λ c
   siblings = Map.toUnfoldable λ # L.filter (\(c' × _) -> isCtr λ c' && rootClass λ c' == r)
   sigOf (c' × (mb × xs)) = c' × (inherited + List.length xs)
      where
      inherited = maybe 0 (\b -> maybe 0 (List.length <<< snd) (Map.lookup b λ)) mb

arity :: ClassCtx -> Ctr -> Maybe Int
arity λ c = do
   DataType _ sigs <- dataType λ c
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

