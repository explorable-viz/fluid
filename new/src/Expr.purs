module Expr where

import Prelude hiding (join)
import Data.List (List, (:))
import Data.List (List(..)) as L
import Data.Maybe (Maybe(..))
import Bindings (Var)
import Selected (Selected(..))
import Util (error)

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
   ElimBool { true :: k, false :: k } |
   ElimPair (Elim (Elim k)) |
   ElimList { nil :: k, cons :: Elim (Elim k) }

instance elimFunctor :: Functor Elim where
   map f (ElimVar x κ) = ElimVar x (f κ)
   map f (ElimBool { true: κ, false: κ' }) = ElimBool { true: f κ, false: f κ' }
   map f (ElimPair σ) = ElimPair $ map (map f) σ
   map f (ElimList { nil: κ, cons: σ }) = ElimList { nil: f κ, cons: map (map f) σ }

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

instance pElimJoinable :: Joinable k => Joinable (PElim k) where
   join L.Nil = Nothing
   join (b : L.Nil) = Just b
   join (PElimVar x κ : PElimVar x' κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ PElimVar x κ'' : bs -- todo: check x == x'
   join (PElimTrue κ : PElimTrue κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ PElimTrue κ'' : bs
   join (PElimFalse κ : PElimFalse κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ PElimTrue κ'' : bs
   join (PElimBool { true: κ1, false: κ2 } : PElimTrue κ1' : bs) = do
      κ1'' <- join (κ1 : κ1' : L.Nil)
      join $ PElimBool { true: κ1'', false: κ2 } : bs
   join (PElimBool { true: κ1, false: κ2 } : PElimFalse κ2' : bs) = do
      κ2'' <- join (κ2 : κ2' : L.Nil)
      join $ PElimBool { true: κ1, false: κ2'' } : bs
   join (PElimBool { true: κ1, false: κ2 } : PElimBool { true: κ1', false: κ2' } : bs) = do
      κ1'' <- join (κ1 : κ1' : L.Nil)
      κ2'' <- join (κ2 : κ2' : L.Nil)
      join $ PElimBool { true: κ1'', false: κ2'' } : bs
   join (PElimPair σ : PElimPair σ' : bs) = do
      σ'' <- join (σ : σ' : L.Nil)
      join $ PElimPair σ'' : bs
   join (PElimNil κ : PElimNil κ' : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      join $ PElimNil κ'' : bs
   join (PElimCons σ : PElimCons σ' : bs) = do
      σ'' <- join (σ : σ' : L.Nil)
      join $ PElimCons σ'' : bs
   join (PElimList { nil: κ, cons: σ } : PElimList { nil: κ', cons: σ' } : bs) = do
      κ'' <- join (κ : κ' : L.Nil)
      σ'' <- join (σ : σ' : L.Nil)
      join $ PElimList { nil: κ'', cons: σ'' } : bs
   join _ = Nothing

toElim :: forall k . PElim k -> Maybe (Elim k)
toElim (PElimVar x κ) = Just $ ElimVar x κ
toElim (PElimBool { true: κ, false: κ' }) =
   Just $ ElimBool { true: κ, false: κ' }
toElim (PElimPair σ) = do
   σ' <- bibble (map toElim σ) >>= toElim
   Just $ ElimPair σ'
toElim (PElimList { nil: κ, cons: σ }) = do
   σ' <- bibble (map toElim σ) >>= toElim
   Just $ ElimList { nil: κ, cons: σ' }
toElim _ = Nothing

bibble :: forall k . PElim (Maybe k) -> Maybe (PElim k)
bibble (PElimVar x (Just κ)) = Just $ PElimVar x κ
bibble (PElimTrue (Just κ)) = Just $ PElimTrue κ
bibble (PElimFalse (Just κ)) = Just $ PElimFalse κ
bibble (PElimBool { true: Just κ, false: Just κ' }) = Just $ PElimBool { true: κ, false: κ' }
bibble (PElimPair σ) = bibble (map bibble σ) >>= Just <<< PElimPair
bibble (PElimNil (Just κ)) = Just $ PElimNil κ
bibble (PElimCons σ) = bibble (map bibble σ) >>= Just <<< PElimCons
bibble (PElimList { nil: Just κ, cons: σ }) = do
   σ' <- bibble (map bibble σ)
   pure $ PElimList { nil: κ, cons: σ' }
bibble _ = Nothing
