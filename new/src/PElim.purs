module PElim where

import Prelude hiding (absurd, join)
import Data.Bitraversable (bisequence)
import Data.List (List(..), (:))
import Data.Map (singleton, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl, sequence)
import Data.Tuple (Tuple(..))
import DataType (Ctr)
import Expr (Cont(..), Elim(..), Expr)
import Util (type (×), (≟), error, om, unionWithMaybe)

class Joinable k where
   maybeJoin :: k -> k -> Maybe k

instance joinableExpr :: Joinable Expr where
   maybeJoin _ _ = Nothing

instance joinableCont :: Joinable Cont where
   maybeJoin CNone CNone          = pure CNone
   maybeJoin (CExpr e) (CExpr e') = CExpr <$> maybeJoin e e'
   maybeJoin (CElim σ) (CElim σ') = CElim <$> maybeJoin σ σ'
   maybeJoin _ _                  = Nothing

instance joinableCtrCont :: Joinable (Ctr × Cont) where
   maybeJoin (Tuple c κ) (Tuple c' κ') = bisequence $ Tuple (c ≟ c') $ maybeJoin κ κ'

instance joinableElim :: Joinable Elim where
   maybeJoin (ElimVar x κ) (ElimVar x' κ')    = ElimVar <$> x ≟ x' <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs') = ElimConstr <$> (sequence $ unionWithMaybe maybeJoin κs κs')
   maybeJoin _ _ = Nothing

joinAll :: forall a . Joinable a => List a -> Maybe a
joinAll Nil = error "Non-empty list expected"
joinAll (x : xs) = foldl (om maybeJoin) (Just x) xs

class MapCont a where
   mapCont :: Cont -> a -> Maybe a

instance mapContCont :: MapCont Cont where
   mapCont κ CNone      = pure κ
   mapCont κ (CExpr _)  = pure κ
   mapCont κ (CElim σ)  = CElim <$> mapCont κ σ

instance mapContElim :: MapCont Elim where
   mapCont κ (ElimVar x κ') = ElimVar x <$> mapCont κ κ'
   mapCont κ (ElimConstr κs) =
      case toUnfoldable κs of
         Tuple c κ' : Nil -> do
            ElimConstr <$> (singleton c <$> mapCont κ κ')
         _ -> Nothing
