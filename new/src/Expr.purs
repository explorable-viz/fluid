module Expr where

import Prelude hiding (join)
import Data.List (List, (:))
import Data.List (List(..)) as L
import Data.Maybe (Maybe(..))
import Bindings (Var)
import Selected (Selected(..))

data T3 a b c = T3 a b c

-- recursive functions
data Def = Def Var (Elim Expr)
type Defs = List Def

data RawExpr =
   Var Var |
   Op Var |
   Int Int |
   True | False |
   Pair Expr Expr |
   Nil | Cons Expr Expr |
   Lambda (Elim Expr) |
   App Expr Expr |
   BinaryApp Expr Var Expr |
   Match Expr (Elim Expr) |
   Let Var Expr Expr |
   Letrec Defs Expr

data Expr = Expr Selected RawExpr

expr :: RawExpr -> Expr
expr r = Expr Bot r

data Elim k =
   ElimVar Var k |
   ΕlimBool { true :: k, false :: k } |
   ElimPair (Elim (Elim k)) |
   ElimList { nil :: k, cons :: Elim (Elim k) }

instance elimFunctor :: Functor Elim where
   map f (ElimVar x κ) = ElimVar x (f κ)
   map f (ΕlimBool { true: κ, false: κ' }) = ΕlimBool { true: f κ, false: f κ' }
   map f (ElimPair σ) = ElimPair $ map f σ
   map f (ElimList { nil: κ, cons: σ }) = ElimList { nil: f κ, cons: map f σ }

class Joinable k where
   join :: List k -> Maybe k

data Branch k =
   BranchVar Var k |
   BranchTrue k |
   BranchFalse k |
   BranchBool { true :: k, false :: k } |
   BranchPair (Branch (Branch k)) |
   BranchNil k |
   BranchCons (Branch (Branch k)) |
   BranchList { nil :: k, cons :: Branch (Branch k) }

instance branchJoinable :: Joinable k => Joinable (Branch k) where
   join L.Nil = Nothing
   join (b : L.Nil) = Just b
   join (BranchVar x κ : BranchVar x' κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ BranchVar x κ'' : bs -- todo: check x == x'
   join (BranchTrue κ : BranchTrue κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ BranchTrue κ'' : bs
   join (BranchFalse κ : BranchFalse κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ BranchTrue κ'' : bs
   join (BranchBool { true: κ1, false: κ2 } : BranchTrue κ1' : bs) = do
      κ1'' <- join (κ1 : κ1' : L.Nil)
      join $ BranchBool { true: κ1'', false: κ2 } : bs
   join (BranchBool { true: κ1, false: κ2 } : BranchFalse κ2' : bs) = do
      κ2'' <- join (κ2 : κ2' : L.Nil)
      join $ BranchBool { true: κ1, false: κ2'' } : bs
   join (BranchBool { true: κ1, false: κ2 } : BranchBool { true: κ1', false: κ2' } : bs) = do
      κ1'' <- join (κ1 : κ1' : L.Nil)
      κ2'' <- join (κ2 : κ2' : L.Nil)
      join $ BranchBool { true: κ1'', false: κ2'' } : bs
   join (BranchPair σ : BranchPair σ' : bs) = do
      σ'' <- join (σ : σ' : L.Nil)
      join $ BranchPair σ'' : bs
   join (BranchNil κ : BranchNil κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ BranchNil κ'' : bs
   join (BranchCons σ : BranchCons σ' : bs) = do
      σ'' <- join (σ : σ' : L.Nil)
      join $ BranchCons σ'' : bs
   join (BranchList { nil: κ, cons: σ } : BranchList { nil: κ', cons: σ' } : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      σ'' <- join (σ : σ' : L.Nil)
      join $ BranchList { nil: κ'', cons: σ'' } : bs
   join _ = Nothing

toElim :: forall k . Branch k -> Maybe (Elim k)
toElim (BranchBool { true: κ, false: κ' }) =
   Just $ ΕlimBool { true: κ, false: κ' }
toElim (BranchList { nil: κ, cons: σ }) = ?_
{-
   do
   σ' <- toElim (map toElim σ)
   Just $ ElimList { nil: κ, cons: σ' }
-}
toElim _ = Nothing
