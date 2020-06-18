module PElim where

import Prelude hiding (absurd, join)
import Data.Bitraversable (bisequence)
import Data.List (List(..), (:))
import Data.Map (singleton, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl, sequence)
import Data.Tuple (Tuple(..))
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont(..), Elim(..), Expr)
import Util (type (×), (≟), error, om, unionWithMaybe)

class Joinable k where
   maybeJoin :: k -> k -> Maybe k

instance joinableExpr :: Joinable Expr where
   maybeJoin _ _ = Nothing

instance joinableCont :: Joinable Cont where
   maybeJoin None None              = pure None
   maybeJoin (Body e) (Body e')     = Body <$> maybeJoin e e'
   maybeJoin (Arg n σ) (Arg m σ')   = Arg <$> n ≟ m <*> maybeJoin σ σ'
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

data PCont = PNone | PBody Expr | PArg Int Pattern

data Pattern =
   PattVar Var PCont |
   PattConstr Ctr PCont

class MapCont2 a where
   mapCont2 :: PCont -> a -> a

instance mapCont2Cont :: MapCont2 PCont where
   mapCont2 κ PNone       = κ
   mapCont2 κ (PBody _)   = κ
   mapCont2 κ (PArg n σ)  = PArg n $ mapCont2 κ σ

instance mapCont2Elim :: MapCont2 Pattern where
   mapCont2 κ (PattVar x κ')     = PattVar x $ mapCont2 κ κ'
   mapCont2 κ (PattConstr c κ')  = PattConstr c $ mapCont2 κ κ'
