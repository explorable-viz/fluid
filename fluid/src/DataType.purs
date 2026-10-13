module DataType where

import Prelude hiding (absurd, join)
import Prim hiding (Type)

import Bind (Name, Var, qual)
import ModuleGraph (builtins, prelude)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Data.Bifunctor (rmap)
import Data.Foldable (all, and, any, foldl, foldr, lookup) as F
import Data.List (List(..), elem, elemIndex, filter, index, length, nubEq, null, zipWith, (:))
import Data.List.NonEmpty (NonEmptyList(..), last) as NE
import Data.NonEmpty ((:|))
import Data.Map (Map, empty, singleton, unionWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple (fst)
import Effect.Exception (Error)
import Types (Primitive(..), Type(..), baseType, fv, subst, subterms)
import Util (type (×), absurd, error, throw, (×), (∩))
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

-- Type arguments of ancestor d of C[τs].
ancestorAt :: ClassTable -> Name -> List Type -> Name -> List Type
ancestorAt classes c τs d
   | c == d = τs
   | Just (ClassTy c' σs) <- baseAt classes c τs = ancestorAt classes c' σs d
   | otherwise = error absurd

fieldsAt :: ClassTable -> Name -> List Type -> List (Var × Type)
fieldsAt classes c τs = rmap (subst τs (get c classes).typeParams) <$> fields classes c

data Instance = Instance (List Type) | Undetermined

instanceAt :: ClassTable -> Name -> Type -> Maybe Instance
instanceAt classes c τ = instanceAbove τ <|> instanceBelow τ
   where
   αs = (get c classes).typeParams
   monomorphic = if null αs then Instance Nil else Undetermined

   instanceAbove (ClassTy d τs) | c `elem` ancestors classes d = Just (Instance (ancestorAt classes d τs c))
   instanceAbove (PrimitiveTy Never) = Just monomorphic
   instanceAbove _ = Nothing

   instanceBelow (ClassTy d σs) | d `elem` ancestors classes c =
      case filter solves candidates of
         Nil -> Nothing
         τs : τss
            | F.all (F.and <<< zipWith (equiv classes) τs) τss -> Just (Instance τs)
            | otherwise -> Just Undetermined
      where
      ρs = ancestorAt classes c (VarTy <$> αs) d
      atoms = nubEq (PrimitiveTy Object : (σs >>= subterms))
      candidates = sequence (αs <#> \_ -> joins classes <$> subsets atoms)
      solves τs = F.and (zipWith (\ρ σ -> equiv classes (subst τs αs ρ) σ) ρs σs)
   instanceBelow (PrimitiveTy Object) = Just monomorphic
   instanceBelow _ = Nothing

subsets :: forall a. List a -> List (List a)
subsets Nil = pure Nil
subsets (x : xs) = subsets xs >>= \ys -> ys : (x : ys) : Nil

comparable :: ClassTable -> Type -> Type -> Boolean
comparable classes σ τ = subtype classes σ τ || subtype classes τ σ

class TypeArgs a where
   typeArgs :: ClassTable -> List Var -> a -> a -> Map Var Type

instance TypeArgs Type where
   typeArgs _ αs (VarTy α) τ | α `elem` αs = singleton α (baseType τ)
   typeArgs classes αs σ (UnionTy τ τ') = joinCxt classes (typeArgs classes αs σ τ) (typeArgs classes αs σ τ')
   typeArgs classes αs (ListTy σ) (ListTy τ) = typeArgs classes αs σ τ
   typeArgs classes αs (DictTy σ) (DictTy τ) = typeArgs classes αs σ τ
   typeArgs classes αs (TupleTy σs) (TupleTy τs) = typeArgs classes αs σs τs
   typeArgs classes αs (CallableTy σs σ) (CallableTy τs τ) = typeArgs classes αs (σ : σs) (τ : τs)
   typeArgs classes αs (TypeTy σ) (TypeTy τ) = typeArgs classes αs σ τ
   typeArgs classes αs (ClassTy c σs) τ = case instanceAt classes c τ of
      Just (Instance τs) -> typeArgs classes αs σs τs
      _ -> empty
   typeArgs classes αs (UnionTy σ σ') τ
      | F.any (\σ'' -> Set.isEmpty (fv σ'' ∩ Set.fromFoldable αs) && subtype classes τ σ'') [ σ, σ' ] = empty
      | otherwise = joinCxt classes (typeArgs classes αs σ τ) (typeArgs classes αs σ' τ)
   typeArgs _ _ _ _ = empty

instance TypeArgs a => TypeArgs (List a) where
   typeArgs classes αs σs τs = F.foldl (joinCxt classes) empty (zipWith (typeArgs classes αs) σs τs)

joinCxt :: ClassTable -> Map Var Type -> Map Var Type -> Map Var Type
joinCxt classes = unionWith (join classes)

elemType :: ClassTable -> Type -> Maybe Type
elemType _ τ | baseType τ == PrimitiveTy Str = Just (PrimitiveTy Str)
elemType _ (ListTy σ) = Just σ
elemType _ (DictTy _) = Just (PrimitiveTy Str)
elemType _ (ClassTy c Nil) | c == cRange = Just (PrimitiveTy Int)
elemType classes (TupleTy τs) = Just (joins classes (baseType <$> τs))
elemType classes (UnionTy σ τ) = join classes <$> elemType classes σ <*> elemType classes τ
elemType _ _ = Nothing

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
