module Pattern2 where

import Prelude hiding (absurd, join)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map, insert, lookup, singleton, update)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (foldl)
import Bindings (Var)
import DataType (DataType, Ctr, arity, dataTypeFor, typeName)
import Expr (Cont2, Cont2'(..), Elim2, Elim2'(..), Expr2, Expr2'(..), RawExpr2(..), expr2)
import Util (MayFail, (≞), (=<<<), absurd, error, om, report, with)

data PCont =
   PNone |              -- intermediate state during construction, but also for structured let
   PBody Expr2 |
   PLambda Pattern |    -- unnecessary if surface language supports piecewise definitions
   PArg Pattern

toCont :: PCont -> MayFail Cont2
toCont PNone         = pure None2
toCont (PBody e)     = pure $ Body2 e
toCont (PLambda π)   = Body2 <$> (expr2 <$> (Lambda2 <$> toElim π))
toCont (PArg π)      = Arg2 <$> toElim π

data Pattern =
   PattVar Var PCont |
   PattConstr Ctr Int PCont

toElim :: Pattern -> MayFail Elim2
toElim (PattVar x κ)      = ElimVar2 x <$> toCont κ
toElim (PattConstr c n κ) = checkArity c n *> (ElimConstr2 <$> (singleton c <$> toCont κ))

class MapCont a where
   -- replace a None continuation by a non-None one
   setCont :: PCont -> a -> a

instance setContPCont :: MapCont PCont where
   setCont κ PNone         = κ
   setCont κ (PBody _)     = error absurd
   setCont κ (PLambda π)   = PLambda $ setCont κ π
   setCont κ (PArg π)      = PArg $ setCont κ π

instance setContPattern :: MapCont Pattern where
   setCont κ (PattVar x κ')      = PattVar x $ setCont κ κ'
   setCont κ (PattConstr c n κ') = PattConstr c n $ setCont κ κ'

class Joinable a b | a -> b where
   maybeJoin :: b -> a -> MayFail b

dataType :: Map Ctr Cont2 -> MayFail DataType
dataType κs = case keys κs of
   Nil   -> error absurd
   c : _ -> dataTypeFor c

instance joinablePatternElim :: Joinable Pattern (Elim2' Boolean) where
   maybeJoin (ElimVar2 x κ) (PattVar y κ')       = ElimVar2 <$> x ≞ y <*> maybeJoin κ κ'
   maybeJoin (ElimConstr2 κs) (PattConstr c n κ) = ElimConstr2 <$> mayFailUpdate
      where
      mayFailUpdate :: MayFail (Map Ctr Cont2)
      mayFailUpdate =
         case lookup c κs of
            Nothing -> do
               checkDataType
               insert <$> pure c <*> toCont κ <@> κs
               where
               checkDataType :: MayFail Unit
               checkDataType = void $ do
                  (with "Non-uniform patterns" $
                     (typeName <$> dataType κs) `(=<<<) (≞)` (typeName <$> dataTypeFor c))
                  *> checkArity c n
            Just κ' -> update <$> (const <$> pure <$> maybeJoin κ' κ) <@> c <@> κs
   maybeJoin _ _                               = report "Can't join variable and constructor patterns"

instance joinablePContCont :: Joinable PCont (Cont2' Boolean) where
   maybeJoin None2 PNone                               = pure None2
   maybeJoin (Arg2 σ) (PArg π)                         = Arg2 <$> maybeJoin σ π
   maybeJoin (Body2 (Expr2' _ (Lambda2 σ))) (PLambda π)   = Body2<$> (expr2 <$> (Lambda2 <$> maybeJoin σ π))
   maybeJoin _ _                                      = report "Incompatible continuations"

joinAll :: NonEmptyList Pattern -> MayFail Elim2
joinAll (NonEmptyList (π :| πs)) = foldl (om $ maybeJoin) (toElim π) πs

checkArity :: Ctr -> Int -> MayFail Int
checkArity c n = with ("Checking arity of " <> show c) $ arity c `(=<<<) (≞)` pure n
