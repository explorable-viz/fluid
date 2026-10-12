module DataType where

import Prelude hiding (join)
import Prim hiding (Type)

import Bind (Name, Var, qual)
import ModuleGraph (builtins, prelude)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Data.Bifunctor (rmap)
import Data.Foldable (and, foldr, lookup) as F
import Data.List (List(..), elemIndex, index, length, zipWith, (:))
import Data.List.NonEmpty (NonEmptyList(..), last) as NE
import Data.NonEmpty ((:|))
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Effect.Exception (Error)
import Types (Primitive(..), Type(..), baseType, subst)
import Util (type (×), definitely', throw, (×))
import Util.Map (get, lookup)

type FieldName = String
type Ctr = String -- newtype would require more general Dict keys

type ClassDef =
   { typeParams :: List Var
   , fields :: List (Var × Type) -- declared fields, distinct
   , base :: Maybe (Name × List Type)
   }

type ClassTable = Map Name ClassDef

fields :: ClassTable -> Name -> List (Var × Type)
fields classes c = case def.base of
   Nothing -> def.fields
   Just (d × σs) -> (rmap (subst σs (get d classes).typeParams) <$> fields classes d) <> def.fields
   where
   def = get c classes

fieldNames :: ClassTable -> Name -> List Var
fieldNames classes c = fst <$> fields classes c

ancestors :: ClassTable -> Name -> List Name
ancestors classes c = c : maybe Nil (ancestors classes <<< fst) (get c classes).base

baseAt :: ClassTable -> Name -> List Type -> Maybe Type
baseAt classes c τs = def.base <#> \(d × σs) -> ClassTy d (subst τs def.typeParams <$> σs)
   where
   def = get c classes

fieldMap :: forall a. ClassTable -> Name -> List a -> List (Var × a) -> Var -> Maybe a
fieldMap classes c xs xys x = case elemIndex x (fst <$> fields classes c) of
   Just i | i < length xs -> index xs i
   _ -> F.lookup x xys

class HasClasses m where
   askClasses :: m ClassTable

instance (Monad m, HasClasses m) => HasClasses (StateT s m) where
   askClasses = lift askClasses

instance (Monad m, HasClasses m) => HasClasses (ReaderT r m) where
   askClasses = lift askClasses

instance (Monad m, HasClasses m) => HasClasses (ExceptT e m) where
   askClasses = lift askClasses

instance (Monad m, HasClasses m, Monoid w) => HasClasses (WriterT w m) where
   askClasses = lift askClasses

arity :: forall m. MonadError Error m => Name -> ClassTable -> m Int
arity c classes = case lookup c classes of
   Nothing -> throw $ "Unknown dataclass: " <> NE.last c
   Just _ -> pure (length (fields classes c))

checkArity :: forall m. MonadError Error m => Name -> Int -> ClassTable -> m Unit
checkArity c n classes = do
   n' <- arity c classes
   when (n' /= n) $ throw $ NE.last c <> " arity " <> show n' <> "; got " <> show n

subtype :: ClassTable -> Type -> Type -> Boolean
subtype classes σ τ
   | σ == τ = true
   | otherwise =
        case σ, τ of
           UnionTy σ1 σ2, _ -> subtype classes σ1 τ && subtype classes σ2 τ
           _, UnionTy τ1 τ2 -> subtype classes σ τ1 || subtype classes σ τ2
           PrimitiveTy Never, _ -> true
           _, PrimitiveTy Object -> true
           PrimitiveTy Int, PrimitiveTy Float -> true
           LitTy _, _ -> subtype classes (baseType σ) τ
           ClassTy c σs, ClassTy d τs | c == d -> F.and (zipWith (equiv classes) σs τs)
           ClassTy c σs, _ | Just base <- baseAt classes c σs -> subtype classes base τ
           _, PrimitiveTy Sized -> sized σ
           ListTy σ', ListTy τ' -> equiv classes σ' τ'
           DictTy σ', DictTy τ' -> equiv classes σ' τ'
           TypeTy σ', TypeTy τ' -> subtype classes σ' τ'
           TupleTy σs, TupleTy τs -> length σs == length τs && F.and (zipWith (subtype classes) σs τs)
           CallableTy σs σ', CallableTy τs τ' ->
              length σs == length τs && F.and (zipWith (subtype classes) τs σs) && subtype classes σ' τ'
           _, _ -> false
        where
        sized (ListTy _) = true
        sized (DictTy _) = true
        sized (PrimitiveTy Str) = true
        sized (TupleTy _) = true
        sized (ClassTy c Nil) = c == cRange
        sized _ = false

equiv :: ClassTable -> Type -> Type -> Boolean
equiv classes σ τ = subtype classes σ τ && subtype classes τ σ

join :: ClassTable -> Type -> Type -> Type
join classes σ τ
   | subtype classes σ τ = τ
   | subtype classes τ σ = σ
   | otherwise = UnionTy σ τ

joins :: ClassTable -> List Type -> Type
joins classes = F.foldr (join classes) (PrimitiveTy Never)

meet :: ClassTable -> Type -> Type -> Type
meet classes σ τ
   | subtype classes σ τ = σ
   | subtype classes τ σ = τ
   | otherwise =
        case σ, τ of
           UnionTy σ1 σ2, _ -> join classes (meet classes σ1 τ) (meet classes σ2 τ)
           _, UnionTy τ1 τ2 -> join classes (meet classes σ τ1) (meet classes σ τ2)
           CallableTy σs σ', CallableTy τs τ' | length σs == length τs ->
              CallableTy (zipWith (join classes) σs τs) (meet classes σ' τ')
           TupleTy σs, TupleTy τs | length σs == length τs -> TupleTy (zipWith (meet classes) σs τs)
           _, _ -> PrimitiveTy Never

fieldIndex :: ClassTable -> Name -> FieldName -> Int
fieldIndex classes c field = definitely' (elemIndex field (fst <$> fields classes c))

view :: Name
view = NE.NonEmptyList ("fluid" :| "view" : Nil)

-- Used internally by primitives, desugaring or rendering layer.
cDefault = qual view "Default" :: Name -- Orientation
cRotated = qual view "Rotated" :: Name
cBarChart = qual view "BarChart" :: Name -- View
cLineChart = qual view "LineChart" :: Name
cLinePlot = qual view "LinePlot" :: Name
cMultiView = qual view "MultiView" :: Name
cScatterPlot = qual view "ScatterPlot" :: Name
cParagraph = qual view "Paragraph" :: Name
cDimensions = qual view "Dimensions" :: Name
cPoint = qual view "Point" :: Name
cAxisLabels = qual view "AxisLabels" :: Name
cTickLabels = qual view "TickLabels" :: Name
cSegment = qual view "Segment" :: Name
cStackedBar = qual view "StackedBar" :: Name
cNonEmpty = qual prelude "NonEmpty" :: Name -- Tree
cRange = qual builtins "range" :: Name
cMatrix = qual builtins "Matrix" :: Name
cText = qual view "Text" :: Name
cLink = qual view "Link" :: Name
-- Field names used internally by rendering layer.
f_caption = "caption" :: FieldName
f_fragments = "fragments" :: FieldName
f_label = "label" :: FieldName
f_text = "text" :: FieldName
f_value = "value" :: FieldName
f_colour = "c" :: FieldName
f_left = "left" :: FieldName
f_right = "right" :: FieldName
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
