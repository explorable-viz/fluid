module PElim where

import Prelude hiding (absurd, join)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map, insert, lookup, singleton, update)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (foldl)
import Bindings (Var)
import DataType (Ctr)
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), expr)
import Util ((≟), om)

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

class MapCont a where
   mapCont :: PCont -> a -> a

instance mapPContCont :: MapCont PCont where
   mapCont κ PNone        = κ
   mapCont κ (PBody _)    = κ
   mapCont κ (PLambda π)  = PLambda $ mapCont κ π
   mapCont κ (PArg n π)   = PArg n $ mapCont κ π

instance mapContPattern :: MapCont Pattern where
   mapCont κ (PattVar x κ')     = PattVar x $ mapCont κ κ'
   mapCont κ (PattConstr c κ')  = PattConstr c $ mapCont κ κ'

class Joinable a b | a -> b where
   maybeJoin :: b -> a -> Maybe b

maybeUpdate :: Ctr -> PCont -> Map Ctr Cont -> Maybe (Map Ctr Cont)
maybeUpdate c κ κs =
   case lookup c κs of
      Nothing -> insert <$> pure c <@> toCont κ <@> κs
      Just κ' -> update <$> (const <$> (Just <$> maybeJoin κ' κ)) <@> c <@> κs

instance joinablePatternElim :: Joinable Pattern Elim where
   maybeJoin (ElimVar x κ) (PattVar y κ')      = ElimVar <$> x ≟ y <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (PattConstr c κ)  = ElimConstr <$> maybeUpdate c κ κs
   maybeJoin _ _                               = Nothing

instance joinablePContCont :: Joinable PCont Cont where
   maybeJoin None PNone                              = pure None
   maybeJoin (Arg n σ) (PArg m π)                    = Arg <$> (n ≟ m) <*> maybeJoin σ π
   maybeJoin (Body (Expr _ (Lambda σ))) (PLambda π)  = Body <$> (expr <$> (Lambda <$> maybeJoin σ π))
   maybeJoin _ _                                     = Nothing

joinAll :: NonEmptyList Pattern -> Maybe Elim
joinAll (NonEmptyList (π :| πs)) = foldl (om maybeJoin) (Just $ toElim π) πs
