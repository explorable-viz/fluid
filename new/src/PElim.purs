module PElim where

import Prelude hiding (absurd, join)
import Data.Bitraversable (bisequence)
import Data.Either (Either(..))
import Data.List (List(..), (:), zipWith)
import Data.Map (Map, fromFoldable, keys, singleton, toUnfoldable, unionWith, values)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl, sequence)
import Data.Tuple (Tuple(..))
import Bindings (Var)
import DataType (Ctr)
import Elim (Elim(..))
import Expr (Cont(..), Elim2(..), Expr)
import Util (type (×), (≟), absurd, assert, error)

-- A "partial" eliminator. A convenience for the parser, which must assemble eliminators out of these.
data PElim k =
   PElimVar Var k |
   PElimTrue k |
   PElimFalse k |
   PElimBool { true :: k, false :: k } |
   PElimPair (PElim (PElim k)) |
   PElimNil k |
   PElimCons (PElim (PElim k)) |
   PElimList { nil :: k, cons :: PElim (PElim k) }

derive instance pElimFunctor :: Functor PElim

-- A "partial" eliminator. A convenience for the parser, which must assemble eliminators out of these.
data PCont = None | PCExpr Expr | PCPElim PElim2

data PElim2 =
   PElimVar2 Var PCont |
   PElimConstr (Map Ctr PCont)

class Joinable k where
   join :: List k -> Maybe k

class Joinable2 k where
   join2 :: k -> k -> Maybe k

instance exprJoinable :: Joinable Expr where
   join _ = Nothing

instance joinableExpr :: Joinable2 Expr where
   join2 _ _ = Nothing

-- This will simplify into a more generic style once we reinstate arbitrary data types.
instance pElimJoinable :: Joinable k => Joinable (PElim k) where
   join Nil = Nothing
   join (b : Nil) = Just b
   join (PElimVar x κ : PElimVar x' κ' : bs) = do
      x'' <- x ≟ x'
      κ'' <- join (κ : κ' : Nil)
      join $ PElimVar x κ'' : bs
   join (PElimTrue κ : PElimTrue κ' : bs) = do
      κ'' <- join (κ : κ' : Nil)
      join $ PElimTrue κ'' : bs
   join (PElimFalse κ : PElimFalse κ' : bs) = do
      κ'' <- join (κ : κ' : Nil)
      join $ PElimTrue κ'' : bs
   join (PElimTrue κ : PElimFalse κ' : bs) =
      join $ PElimBool { true: κ, false: κ' } : bs
   join (PElimFalse κ : PElimTrue κ' : bs) =
      join $ PElimBool { true: κ', false: κ } : bs
   join (PElimBool { true: κ1, false: κ2 } : PElimTrue κ1' : bs) = do
      κ1'' <- join (κ1 : κ1' : Nil)
      join $ PElimBool { true: κ1'', false: κ2 } : bs
   join (PElimBool { true: κ1, false: κ2 } : PElimFalse κ2' : bs) = do
      κ2'' <- join (κ2 : κ2' : Nil)
      join $ PElimBool { true: κ1, false: κ2'' } : bs
   join (_ : PElimBool { true: κ1', false: κ2' } : bs) =
      error absurd
   join (PElimPair σ : PElimPair σ' : bs) = do
      σ'' <- join (σ : σ' : Nil)
      join $ PElimPair σ'' : bs
   join (PElimNil κ : PElimNil κ' : bs) = do
      κ'' <- join (κ : κ' : Nil)
      join $ PElimNil κ'' : bs
   join (PElimCons σ : PElimCons σ' : bs) = do
      σ'' <- join (σ : σ' : Nil)
      join $ PElimCons σ'' : bs
   join (PElimNil κ : PElimCons σ : bs) =
      join $ PElimList { nil: κ, cons: σ } : bs
   join (PElimCons σ : PElimNil κ : bs) =
      join $ PElimList { nil: κ, cons: σ } : bs
   join (PElimList { nil: κ, cons: σ } : PElimNil κ' : bs) = do
      κ'' <- join (κ : κ' : Nil)
      join $ PElimList { nil: κ'', cons: σ } : bs
   join (PElimList { nil: κ, cons: σ } : PElimCons σ' : bs) = do
      σ'' <- join (σ : σ' : Nil)
      join $ PElimList { nil: κ, cons: σ'' } : bs
   join (_ : PElimList { nil: κ', cons: σ } : bs) =
      error absurd
   join (σ : τ : _) = Nothing

instance joinableCont :: Joinable2 PCont where
   join2 None None                  = Nothing
   join2 (PCExpr e) (PCExpr e')     = PCExpr <$> join2 e e'
   join2 (PCPElim σ) (PCPElim σ')   = PCPElim <$> join2 σ σ'
   join2 _ _                        = Nothing

instance joinableCtrCont :: Joinable2 (Ctr × PCont) where
   join2 (Tuple c κ) (Tuple c' κ') = bisequence $ Tuple (c ≟ c') $ join2 κ κ'

instance joinablePElim2 :: Joinable2 PElim2 where
   join2 (PElimVar2 x κ) (PElimVar2 x' κ')   = PElimVar2 <$> x ≟ x' <*> join2 κ κ'
   join2 (PElimConstr κs) (PElimConstr κs')  = PElimConstr <$>
      (fromFoldable <$> sequence (zipWith join2 (toUnfoldable κs) (toUnfoldable κs')))
   join2 _ _ = Nothing

joinAll :: forall a . Joinable2 a => List a -> Maybe a
joinAll = foldl (($>) join2) Nothing

toElim :: forall k . PElim k -> Maybe (Elim k)
toElim (PElimVar x κ)                     = pure $ ElimVar x κ
toElim (PElimBool { true: κ, false: κ' }) = pure $ ElimBool { true: κ, false: κ' }
toElim (PElimPair σ)                      = ElimPair <$> (hoistMaybe (toElim <$> σ) >>= toElim)
toElim (PElimList { nil: κ, cons: σ })    = (\σ' -> ElimList { nil: κ, cons: σ' }) <$>
                                            (hoistMaybe (toElim <$> σ) >>= toElim)
toElim _ = Nothing

toCont :: PCont -> Maybe Cont
toCont None        = Nothing
toCont (PCExpr e)  = CExpr <$> pure e
toCont (PCPElim σ) = CElim <$> toElim2 σ

toElim2 :: PElim2 -> Maybe Elim2
toElim2 (PElimVar2 x κ)    = ElimVar2 x <$> toCont κ
toElim2 (PElimConstr κs)   = ElimConstr <$> sequence (toCont <$> κs)

-- Partial eliminators are not supported at the moment.
singleBranch :: forall k . Elim k -> Maybe k
singleBranch (ElimVar x κ) = Just κ
singleBranch (ElimPair σ)  = singleBranch σ >>= singleBranch
singleBranch _             = Nothing

class SingleBranch a where
   singleBranch2 :: a -> Maybe Cont

instance singleBranchCont :: SingleBranch Cont where
   singleBranch2 (CExpr e) = CExpr <$> pure e
   singleBranch2 (CElim σ) = singleBranch2 σ

instance singleBranchElim :: SingleBranch Elim2 where
   singleBranch2 (ElimVar2 x κ)  = Just κ
   singleBranch2 (ElimConstr κs) =
      case values κs of
         κ : Nil -> singleBranch2 κ
         _ -> Nothing

class MapCont a where
   mapCont :: PCont -> a -> Maybe a

instance mapContCont :: MapCont PCont where
   mapCont κ None          = pure κ
   mapCont κ (PCExpr _)    = pure κ
   mapCont κ (PCPElim σ)   = PCPElim <$> mapCont κ σ

instance mapContElim :: MapCont PElim2 where
   mapCont κ (PElimVar2 x κ') = PElimVar2 x <$> mapCont κ κ'
   mapCont κ (PElimConstr κs) =
      case toUnfoldable κs of
         Tuple c κ' : Nil -> do
            PElimConstr <$> (singleton c <$> mapCont κ κ')
         _ -> Nothing

-- TODO: provide a Traversable instance for PElim; then this is sequence.
hoistMaybe :: forall k . PElim (Maybe k) -> Maybe (PElim k)
hoistMaybe (PElimVar x (Just κ))                         = Just $ PElimVar x κ
hoistMaybe (PElimTrue (Just κ))                          = Just $ PElimTrue κ
hoistMaybe (PElimFalse (Just κ))                         = Just $ PElimFalse κ
hoistMaybe (PElimBool { true: Just κ, false: Just κ' })  = Just $ PElimBool { true: κ, false: κ' }
hoistMaybe (PElimPair σ)                                 = hoistMaybe (hoistMaybe <$> σ) >>= Just <<< PElimPair
hoistMaybe (PElimNil (Just κ))                           = Just $ PElimNil κ
hoistMaybe (PElimCons σ)                                 = hoistMaybe (hoistMaybe <$> σ) >>= Just <<< PElimCons
hoistMaybe (PElimList { nil: Just κ, cons: σ })          = do
   σ' <- hoistMaybe (hoistMaybe <$> σ)
   pure $ PElimList { nil: κ, cons: σ' }
hoistMaybe _                                             = Nothing
