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

class Joinable2 k where
   join2 :: k -> k -> Maybe k

instance joinableExpr :: Joinable2 Expr where
   join2 _ _ = Nothing

instance joinableCont :: Joinable2 Cont where
   join2 CNone CNone          = pure CNone
   join2 (CExpr e) (CExpr e') = CExpr <$> join2 e e'
   join2 (CElim σ) (CElim σ') = CElim <$> join2 σ σ'
   join2 _ _                  = Nothing

instance joinableCtrCont :: Joinable2 (Ctr × Cont) where
   join2 (Tuple c κ) (Tuple c' κ') = bisequence $ Tuple (c ≟ c') $ join2 κ κ'

instance joinableElim :: Joinable2 Elim where
   join2 (ElimVar x κ) (ElimVar x' κ')    = ElimVar <$> x ≟ x' <*> join2 κ κ'
   join2 (ElimConstr κs) (ElimConstr κs') = ElimConstr <$> (sequence $ unionWithMaybe join2 κs κs')
   join2 _ _ = Nothing

joinAll :: forall a . Joinable2 a => List a -> Maybe a
joinAll Nil = error "Non-empty list expected"
joinAll (x : xs) = foldl (om join2) (Just x) xs

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
