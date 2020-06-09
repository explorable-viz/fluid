module PElim where

import Prelude hiding (join)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Bindings (Var)
import Expr (Expr, Elim(..))
import Util ((≟))

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

instance pElimFunctor :: Functor PElim where
   map f (PElimVar x κ) = PElimVar x (f κ)
   map f (PElimTrue κ) = PElimTrue (f κ)
   map f (PElimFalse κ) = PElimFalse (f κ)
   map f (PElimBool { true: κ, false: κ' }) = PElimBool { true: f κ, false: f κ' }
   map f (PElimPair σ) = PElimPair $ map (map f) σ
   map f (PElimNil κ) = PElimNil (f κ)
   map f (PElimCons σ) = PElimCons $ map (map f) σ
   map f (PElimList { nil: κ, cons: σ }) = PElimList { nil: f κ, cons: map (map f) σ }

class Joinable k where
   join :: List k -> Maybe k

instance exprJoinable :: Joinable Expr where
   join _ = Nothing

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
   join (PElimBool { true: κ1, false: κ2 } : PElimBool { true: κ1', false: κ2' } : bs) = do
      κ1'' <- join (κ1 : κ1' : Nil)
      κ2'' <- join (κ2 : κ2' : Nil)
      join $ PElimBool { true: κ1'', false: κ2'' } : bs
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
   join (PElimList { nil: κ, cons: σ } : PElimList { nil: κ', cons: σ' } : bs) = do
      κ'' <- join (κ : κ' : Nil)
      σ'' <- join (σ : σ' : Nil)
      join $ PElimList { nil: κ'', cons: σ'' } : bs
   join _ = Nothing

toElim :: forall k . PElim k -> Maybe (Elim k)
toElim (PElimVar x κ) = Just $ ElimVar x κ
toElim (PElimBool { true: κ, false: κ' }) =
   Just $ ElimBool { true: κ, false: κ' }
toElim (PElimPair σ) = do
   σ' <- hoistMaybe (toElim <$> σ) >>= toElim
   Just $ ElimPair σ'
toElim (PElimList { nil: κ, cons: σ }) = do
   σ' <- hoistMaybe (toElim <$> σ) >>= toElim
   Just $ ElimList { nil: κ, cons: σ' }
toElim _ = Nothing

-- Partial eliminators are not supported at the moment.
singleBranch :: forall k . Elim k -> Maybe k
singleBranch (ElimVar x κ) = Just κ
singleBranch (ElimPair σ) = singleBranch σ >>= singleBranch
singleBranch _ = Nothing

hoistMaybe :: forall k . PElim (Maybe k) -> Maybe (PElim k)
hoistMaybe (PElimVar x (Just κ)) = Just $ PElimVar x κ
hoistMaybe (PElimTrue (Just κ)) = Just $ PElimTrue κ
hoistMaybe (PElimFalse (Just κ)) = Just $ PElimFalse κ
hoistMaybe (PElimBool { true: Just κ, false: Just κ' }) = Just $ PElimBool { true: κ, false: κ' }
hoistMaybe (PElimPair σ) = hoistMaybe (hoistMaybe <$> σ) >>= Just <<< PElimPair
hoistMaybe (PElimNil (Just κ)) = Just $ PElimNil κ
hoistMaybe (PElimCons σ) = hoistMaybe (hoistMaybe <$> σ) >>= Just <<< PElimCons
hoistMaybe (PElimList { nil: Just κ, cons: σ }) = do
   σ' <- hoistMaybe (hoistMaybe <$> σ)
   pure $ PElimList { nil: κ, cons: σ' }
hoistMaybe _ = Nothing
