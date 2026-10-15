module WellFormed.Shapes where

import Prelude hiding (absurd)
import Prim hiding (Type)

import Bind (Name, Var)
import Classes (ClassTable, Instance(..), ancestors, fieldsAt, instanceAt, instanceBelow, meet, subtype)
import Control.Alt ((<|>))
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (any, find)
import Data.List (List(..), drop, elem, filter, length, zip, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import Expr (Pattern(..))
import Literal (Literal(..))
import Types (Primitive(Never), Type(..))
import Types.Defs (bool, none)
import Util (type (×), absurd, error, (×))

data Head
   = HLit Literal
   | HClass Name
   | HList Int

derive instance Eq Head

data Shape
   = Rest Type (List Head)
   | Constr Name (List Type) (List Shape) (List Head)
   | Tuple (List Shape)
   | List Type (List Shape)
   | Dict Type (Map Var Shape) (List Var)

derive instance Eq Shape

shapes :: ClassTable -> Type -> List Head -> List Shape
shapes classes (UnionTy σ τ) hs = ks <> filter (not <<< (_ `elem` ks)) (shapes classes τ (typedHeads classes hs τ))
   where
   ks = shapes classes σ (typedHeads classes hs σ)
shapes _ (LitTy ℓ) hs | HLit ℓ `elem` hs = Nil
shapes _ τ hs | τ == bool, HLit (Bool true) `elem` hs, HLit (Bool false) `elem` hs = Nil
shapes _ τ hs | τ == none, HLit None `elem` hs = Nil
shapes classes (ClassTy c _) hs | belowExcluded classes c hs = Nil
shapes _ (PrimitiveTy Never) _ = Nil
shapes _ (DictTy τ) Nil = Dict τ Map.empty Nil : Nil
shapes _ (DictTy _) _ = error absurd
shapes _ τ hs = Rest τ hs : Nil

shapesSeq :: ClassTable -> List Type -> List (List Shape)
shapesSeq classes τs = sequence (τs <#> \τ -> shapes classes τ Nil)

headTyped :: ClassTable -> Head -> Type -> Boolean
headTyped classes (HLit ℓ) τ = subtype classes (LitTy ℓ) τ
headTyped classes (HClass c) τ = isJust (instanceBelow classes c τ)
headTyped _ (HList _) (ListTy _) = true
headTyped _ (HList _) _ = false

typedHeads :: ClassTable -> List Head -> Type -> List Head
typedHeads classes hs τ = filter (\h -> headTyped classes h τ) hs

belowExcluded :: ClassTable -> Name -> List Head -> Boolean
belowExcluded classes c hs = any (\d -> HClass d `elem` hs) (ancestors classes c)

-- Matched shapes and residual; nothing if p matches no shape
matches :: ClassTable -> Shape -> Pattern -> Maybe (List Shape × List Shape)
matches _ k (PVar _) = Just ((k : Nil) × Nil)
matches _ k PWild = Just ((k : Nil) × Nil)
matches classes k (PAs p _) = matches classes k p
matches classes k p = matchesHead classes k p <|> matchesSplit classes k p

-- Pattern at shape with head tested by pattern
matchesHead :: ClassTable -> Shape -> Pattern -> Maybe (List Shape × List Shape)
matchesHead _ k@(Rest (LitTy ℓ) _) (PLit ℓ') | ℓ == ℓ' = Just ((k : Nil) × Nil)
matchesHead classes (Tuple ks) (PTuple ps) | length ks == length ps = forms Tuple <$> matchesSeq classes ks ps
matchesHead classes (List τ ks) (PList ps) | length ks == length ps = forms (List τ) <$> matchesSeq classes ks ps
matchesHead classes (Dict τ β hs) (PRecord wps) = do
   ks <- traverse (\(w × _) -> Map.lookup w β) wps
   forms (\ks' -> Dict τ (Map.fromFoldable (zip (fst <$> wps) ks') `Map.union` β) hs)
      <$> matchesSeq classes ks (snd <$> wps)
matchesHead classes (Constr c τs ks hs) (PConstr d ps _) | d `elem` ancestors classes c =
   forms (\ks' -> Constr c τs ks' hs) <$> matchesSeq classes ks (ps <> replicate (length ks - length ps) PWild)
matchesHead _ _ _ = Nothing

forms :: (List Shape -> Shape) -> List (List Shape) × List (List Shape) -> List Shape × List Shape
forms form = bimap (map form) (map form)

matchesSplit :: ClassTable -> Shape -> Pattern -> Maybe (List Shape × List Shape)
matchesSplit classes k p = do
   ks × ls <- splits classes k p
   ks' × ls' <- matchesShapes classes ks p
   pure (ks' × (ls <> ls'))

matchesSeq :: ClassTable -> List Shape -> List Pattern -> Maybe (List (List Shape) × List (List Shape))
matchesSeq _ Nil Nil = Just ((Nil : Nil) × Nil)
matchesSeq classes (k : ks) (p : ps) = do
   matched × residual <- matches classes k p
   matched' × residual' <- matchesSeq classes ks ps
   pure (((:) <$> matched <*> matched') × (((_ : ks) <$> residual) <> ((:) <$> matched <*> residual')))
matchesSeq _ _ _ = error absurd

matchesShapes :: ClassTable -> List Shape -> Pattern -> Maybe (List Shape × List Shape)
matchesShapes classes ls p = case partitionMap (\k -> maybe (Left k) Right (matches classes k p)) ls of
   { right: Nil } -> Nothing
   { left: unmatched, right: kls } -> Just ((kls >>= fst) × ((kls >>= snd) <> unmatched))

splits :: ClassTable -> Shape -> Pattern -> Maybe (List Shape × List Shape)
splits _ (Rest (LitTy _) _) (PLit _) = Nothing
splits classes (Rest τ hs) (PLit ℓ)
   | not (HLit ℓ `elem` hs), subtype classes (LitTy ℓ) τ =
        Just ((Rest (LitTy ℓ) Nil : Nil) × shapes classes τ (HLit ℓ : hs))
splits classes (Rest (TupleTy τs) _) (PTuple ps)
   | length τs == length ps = Just ((Tuple <$> shapesSeq classes τs) × Nil)
splits classes (Rest τ@(ListTy σ) hs) (PList ps)
   | not (HList (length ps) `elem` hs) =
        Just ((List σ <$> shapesSeq classes (replicate (length ps) σ)) × shapes classes τ (HList (length ps) : hs))
splits classes (Dict τ β hs) (PRecord wps) = case find (\w -> not (Map.member w β)) (fst <$> wps) of
   Just w | not (w `elem` hs) ->
      Just ((shapes classes τ Nil <#> \k -> Dict τ (Map.insert w k β) hs) × (Dict τ β (w : hs) : Nil))
   _ -> Nothing
splits classes (Rest τ hs) (PConstr c _ _)
   | not (belowExcluded classes c hs)
   , Just (Instance τs) <- instanceAt classes c τ
   , ClassTy d σs <- meet classes τ (ClassTy c τs) =
        Just (constrShapes classes d σs Nil hs × shapes classes τ (HClass c : hs))
splits classes (Constr c τs ks hs) (PConstr d _ _)
   | d /= c
   , not (belowExcluded classes d hs)
   , Just (Instance σs) <- instanceAt classes d (ClassTy c τs)
   , subtype classes (ClassTy d σs) (ClassTy c τs) =
        Just (constrShapes classes d σs ks hs × (Constr c τs ks (HClass d : hs) : Nil))
splits _ _ _ = Nothing

-- Constructor shapes for d with type arguments σs and leading field shapes ks; excluded heads those of hs typed there
constrShapes :: ClassTable -> Name -> List Type -> List Shape -> List Head -> List Shape
constrShapes classes d σs ks hs =
   shapesSeq classes (drop (length ks) (snd <$> fieldsAt classes d σs)) <#> \ks' ->
      Constr d σs (ks <> ks') (typedHeads classes hs (ClassTy d σs))
