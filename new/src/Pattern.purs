module Pattern where

import Prelude hiding (absurd, join)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map, insert, lookup, singleton, update)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (foldl)
import DataType (DataType, Ctr, arity, dataTypeFor, typeName)
import Expr (Cont(..), Elim(..), Expr(..), RawExpr(..), Var, expr)
import Lattice (𝔹)
import Util (MayFail, (≞), (=<<<), absurd, error, om, report, with)

data PCont =
   PNone |              -- intermediate state during construction, but also for structured let
   PBody (Expr 𝔹) |
   PLambda Pattern |    -- unnecessary if surface language supports piecewise definitions
   PArg Pattern

toCont :: PCont -> MayFail (Cont 𝔹)
toCont PNone         = pure None
toCont (PBody e)     = pure $ Body e
toCont (PLambda π)   = Body <$> (expr <$> (Lambda <$> toElim π))
toCont (PArg π)      = Arg <$> toElim π

-- Since this includes the continuation, "Branch" might be a better name.
data Pattern =
   PattVar Var PCont |
   PattConstr Ctr Int PCont

toElim :: Pattern -> MayFail (Elim 𝔹)
toElim (PattVar x κ)      = ElimVar x <$> toCont κ
toElim (PattConstr c n κ) = checkArity c n *> (ElimConstr <$> (singleton c <$> toCont κ))

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

dataType :: Map Ctr (Cont 𝔹) -> MayFail DataType
dataType κs = case keys κs of
   Nil   -> error absurd
   c : _ -> dataTypeFor c

instance joinablePatternElim :: Joinable Pattern (Elim Boolean) where
   maybeJoin (ElimVar x κ) (PattVar y κ')       = ElimVar <$> x ≞ y <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (PattConstr c n κ) = ElimConstr <$> mayFailUpdate
      where
      mayFailUpdate :: MayFail (Map Ctr (Cont 𝔹))
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

instance joinablePContCont :: Joinable PCont (Cont Boolean) where
   maybeJoin None PNone                               = pure None
   maybeJoin (Arg σ) (PArg π)                         = Arg <$> maybeJoin σ π
   maybeJoin (Body (Expr _ (Lambda σ))) (PLambda π)   = Body<$> (expr <$> (Lambda <$> maybeJoin σ π))
   maybeJoin _ _                                      = report "Incompatible continuations"

joinAll :: NonEmptyList Pattern -> MayFail (Elim 𝔹)
joinAll (NonEmptyList (π :| πs)) = foldl (om $ maybeJoin) (toElim π) πs

checkArity :: Ctr -> Int -> MayFail Int
checkArity c n = with ("Checking arity of " <> show c) $ arity c `(=<<<) (≞)` pure n
