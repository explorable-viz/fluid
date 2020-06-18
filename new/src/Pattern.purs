module PElim where

import Prelude hiding (absurd, join)
import Data.Bitraversable (bisequence)
import Data.List (List(..), (:))
import Data.Map (Map, insert, lookup, singleton, toUnfoldable, update)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl, sequence)
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), expr)
import Util (type (×), (×), (≟), error, om, unionWithMaybe)

class Joinable k where
   maybeJoin :: k -> k -> Maybe k

instance joinableCont :: Joinable Cont where
   maybeJoin None None              = pure None
   maybeJoin (Arg n σ) (Arg m σ')   = Arg <$> n ≟ m <*> maybeJoin σ σ'
   maybeJoin (Body e) (Body e')     = Nothing
   maybeJoin _ _                    = Nothing

instance joinableCtrCont :: Joinable (Ctr × Cont) where
   maybeJoin (Tuple c κ) (Tuple c' κ') = bisequence $ Tuple (c ≟ c') $ maybeJoin κ κ'

instance joinableElim :: Joinable Elim where
   maybeJoin (ElimVar x κ) (ElimVar x' κ')    = ElimVar <$> x ≟ x' <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs') = ElimConstr <$> (sequence $ unionWithMaybe maybeJoin κs κs')
   maybeJoin _ _ = Nothing

joinAll :: forall a . Joinable a => List a -> Maybe a
joinAll Nil = error "Non-empty list expected"
joinAll (x : xs) = foldl (om maybeJoin) (Just x) xs

-- Defined only for singleton eliminators.
class MapCont a where
   mapCont :: Cont -> a -> Maybe a

instance mapContCont :: MapCont Cont where
   mapCont κ None       = pure κ
   mapCont κ (Body _)   = pure κ
   mapCont κ (Arg n σ)  = Arg n <$> mapCont κ σ

instance mapContElim :: MapCont Elim where
   mapCont κ (ElimVar x κ') = ElimVar x <$> mapCont κ κ'
   mapCont κ (ElimConstr κs) =
      case toUnfoldable κs of
         Tuple c κ' : Nil -> do
            ElimConstr <$> (singleton c <$> mapCont κ κ')
         _ -> Nothing

data PCont =
   PNone |
   PBody Expr |
   PLambda Pattern |  -- unnecessary if surface language supports piecewise definitions
   PArg Int Pattern

toCont :: PCont -> Cont
toCont PNone         = None
toCont (PBody e)     = Body e
toCont (PLambda π)   = Body $ expr $ Lambda $ toElim π
toCont (PArg n π)    = Arg n $ toElim π

data Pattern =
   PattVar Var PCont |
   PattConstr Ctr PCont

toElim :: Pattern -> Elim
toElim (PattVar x κ)    = ElimVar x $ toCont κ
toElim (PattConstr c κ) = ElimConstr $ singleton c $ toCont κ

class MapCont2 a where
   mapCont2 :: PCont -> a -> a

instance mapCont2Cont :: MapCont2 PCont where
   mapCont2 κ PNone        = κ
   mapCont2 κ (PBody _)    = κ
   mapCont2 κ (PLambda π)  = PLambda $ mapCont2 κ π
   mapCont2 κ (PArg n π)   = PArg n $ mapCont2 κ π

instance mapCont2Elim :: MapCont2 Pattern where
   mapCont2 κ (PattVar x κ')     = PattVar x $ mapCont2 κ κ'
   mapCont2 κ (PattConstr c κ')  = PattConstr c $ mapCont2 κ κ'

class Joinable2 a b | a -> b where
   maybeJoin2 :: b -> a -> Maybe b

maybeUpdate :: Ctr -> PCont -> Map Ctr Cont -> Maybe (Map Ctr Cont)
maybeUpdate c κ κs =
   case lookup c κs of
      Nothing -> insert <$> pure c <@> toCont κ <@> κs
      Just κ' -> update <$> (const <$> (Just <$> maybeJoin2 κ' κ)) <@> c <@> κs

instance joinablePatternElim :: Joinable2 Pattern Elim where
   maybeJoin2 (ElimVar x κ) (PattVar y κ')      = ElimVar <$> x ≟ y <*> maybeJoin2 κ κ'
   maybeJoin2 (ElimConstr κs) (PattConstr c κ)  = ElimConstr <$> maybeUpdate c κ κs
   maybeJoin2 _ _                               = Nothing

instance joinablePContCont :: Joinable2 PCont Cont where
   maybeJoin2 None PNone                              = pure None
   maybeJoin2 (Arg n σ) (PArg m π)                    = Arg <$> (n ≟ m) <*> maybeJoin2 σ π
   maybeJoin2 (Body (Expr _ (Lambda σ))) (PLambda π)  = Body <$> (expr <$> (Lambda <$> maybeJoin2 σ π))
   maybeJoin2 _ _                                     = Nothing

joinAll2 :: List Pattern -> Maybe Elim
joinAll2 Nil      = error "Non-empty list expected"
joinAll2 (π : πs) = foldl (om maybeJoin2) (Just $ toElim π) πs
