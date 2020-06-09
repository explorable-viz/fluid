module Expr where


import Prelude (class Functor, Unit, map, ($), (<$>), (<<<), bind, pure)
import Bindings (Var)
import Data.List (List)
import Data.Either (Either)
import Control.Bind ((>>=))
import Selected
import Util (absurd, error, (≟), mayEq)
import Data.Maybe (Maybe(..))

data Def = Def (Elim Unit) Expr
data RecDef = RecDef Var (Elim Expr)
type RecDefs = List RecDef

data RawExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   True | False |
   Pair Expr Expr |
   Nil | Cons Expr Expr |
   Lambda (Elim Expr) |
   App Expr Expr |
   BinaryApp Expr Var Expr |
   MatchAs Expr (Elim Expr) |
   Let Def Expr |
   LetRec RecDefs Expr

data Expr = Expr Selected RawExpr

expr :: RawExpr -> Expr
expr = Expr Bot


instance rawExprLattice :: Lattice RawExpr where
   maybeJoin (Var x) (Var x') = do
      x'' <- x ≟ x'
      pure (Var x'')
   maybeJoin (Op op) (Op op') = do
      op'' <- op ≟ op'
      pure (Op op'')
   maybeJoin (Int n) (Int n') = do
      n'' <- n ≟ n'
      pure (Int n'')
   maybeJoin (Str s) (Var s') = do
      s'' <- s ≟ s'
      pure (Str s'')
   maybeJoin False False      = pure False
   maybeJoin False True       = pure True
   maybeJoin True False       = pure True
   maybeJoin True True        = pure True
   maybeJoin (Pair e1 e1') (Pair e2 e2') = do
      e   <- e1  ∨? e2
      e'  <- e1' ∨? e2'
      pure (Pair e e')
   maybeJoin Nil Nil = pure Nil
   maybeJoin (Cons e1 e1') (Cons e2 e2') = do
      e   <- e1  ∨? e2
      e'  <- e1' ∨? e2'
      pure (Cons e e')
   maybeJoin _ _ = Nothing

   maybeMeet (Var x) (Var x') = do
      x'' <- x ≟ x'
      pure (Var x'')
   maybeMeet (Op op) (Op op') = do
      op'' <- op ≟ op'
      pure (Op op'')
   maybeMeet (Int n) (Int n') = do
      n'' <- n ≟ n'
      pure (Int n'')
   maybeMeet (Str s) (Var s') = do
      s'' <- s ≟ s'
      pure (Str s'')
   maybeMeet False False      = pure False
   maybeMeet False True       = pure True
   maybeMeet True False       = pure True
   maybeMeet True True        = pure True
   maybeMeet (Pair e1 e1') (Pair e2 e2') = do
      e   <- e1  ∧? e2
      e'  <- e1' ∧? e2'
      pure (Pair e e')
   maybeMeet Nil Nil          = pure Nil
   maybeMeet (Cons e1 e1') (Cons e2 e2') = do
      e   <- e1  ∧? e2
      e'  <- e1' ∧? e2'
      pure (Cons e e')
   maybeMeet _ _ = Nothing

   top e = e
   bot e = e

instance exprLattice :: Lattice Expr where
   maybeJoin (Expr α e) (Expr α' e') = do
      α'' <- α ∨? α'
      e'' <- e ∨? e'
      pure (Expr α'' e'')
   maybeMeet (Expr α e) (Expr α' e') = do
      α'' <- α ∧? α'
      e'' <- e ∧? e'
      pure (Expr α'' e'')
   top (Expr _ r) = Expr Top r
   bot (Expr _ r) = Expr Bot r

data Elim k =
   ElimVar Var k |
   ElimBool { true :: k, false :: k } |
   ElimPair (Elim (Elim k)) |
   ElimList { nil :: k, cons :: Elim (Elim k) }

instance elimFunctor :: Functor Elim where
   map f (ElimVar x κ)                       = ElimVar x (f κ)
   map f (ElimBool { true: κ, false: κ' })   = ElimBool { true: f κ, false: f κ' }
   map f (ElimPair σ)                        = ElimPair $ map (map f) σ
   map f (ElimList { nil: κ, cons: σ })      = ElimList { nil: f κ, cons: map (map f) σ }

instance elimLattice :: Lattice k => Lattice (Elim k) where
   maybeMeet (ElimVar x κ) (ElimVar x' κ') = do
      x'' <- x ≟ x'
      κ'' <- κ ∧? κ'
      pure (ElimVar x'' κ'')
   maybeMeet (ElimBool { true : κ1, false : κ2 }) (ElimBool { true : κ1', false : κ2' }) = do
      κ1'' <- κ1 ∧? κ1'
      κ2'' <- κ2 ∧? κ2'
      pure (ElimBool {true : κ1'', false : κ2''})
   maybeMeet (ElimPair σ) (ElimPair σ')
    = hoistMaybe $ ElimPair (map (\κ -> map (\κ' -> κ ∧? κ') (ElimPair σ')) (ElimPair σ))
   maybeMeet (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 }) = do
      κ <- κ1 ∧? κ2
      σ <- maybeσ
      pure $ ElimList { nil: κ, cons: σ }
      where
         maybeσ = case σ1, σ2 of
                        ElimVar x κ, ElimVar x' κ' -> do
                           x'' <- x ≟ x'
                           κ'' <- κ ∧? κ'
                           pure (ElimVar x'' κ'')
                        ElimBool { true : κ1, false : κ2 }, ElimBool { true : κ1', false : κ2' } -> do
                           κ1'' <- κ1 ∧? κ1'
                           κ2'' <- κ2 ∧? κ2'
                           pure (ElimBool {true : κ1'', false : κ2''})
                        ElimPair σ, ElimPair σ'
                           -> hoistMaybe $ ElimPair (map (\κ -> map (\κ' -> κ ∧? κ') (ElimPair σ')) (ElimPair σ))
                        ElimList { nil: κ1', cons: σ1' }, ElimList { nil: κ2', cons: σ2' } -> do
                           κ  <- κ1' ∧? κ2'
                           σ' <- σ1' ∧? σ2'
                           pure $ ElimList { nil: κ, cons: σ' }
                        _, _ -> Nothing
   maybeMeet _ _ = Nothing

   maybeJoin (ElimVar x κ) (ElimVar x' κ') = do
      x'' <- x ≟ x'
      κ'' <- κ ∨? κ'
      pure (ElimVar x'' κ'')
   maybeJoin (ElimBool { true : κ1, false : κ2 }) (ElimBool { true : κ1', false : κ2' }) = do
      κ1'' <- κ1 ∨? κ1'
      κ2'' <- κ2 ∨? κ2'
      pure (ElimBool {true : κ1'', false : κ2''})
   maybeJoin (ElimPair σ) (ElimPair σ') =
      hoistMaybe $ ElimPair (map (\κ -> map (\κ' -> κ ∨? κ') (ElimPair σ')) (ElimPair σ))
   maybeJoin (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 }) = do
      κ <- κ1 ∨? κ2
      σ <- maybeσ
      pure $ ElimList { nil: κ, cons: σ }
      where
      maybeσ = case σ1, σ2 of
                  ElimVar x κ, ElimVar x' κ' -> do
                     x'' <- x ≟ x'
                     κ'' <- κ ∨? κ'
                     pure (ElimVar x'' κ'')
                  ElimBool { true : κ1, false : κ2 }, ElimBool { true : κ1', false : κ2' } -> do
                     κ1'' <- κ1 ∨? κ1'
                     κ2'' <- κ2 ∨? κ2'
                     pure (ElimBool {true : κ1'', false : κ2''})
                  ElimPair σ, ElimPair σ'
                     -> hoistMaybe $ ElimPair (map (\κ -> map (\κ' -> κ ∨? κ') (ElimPair σ')) (ElimPair σ))
                  ElimList { nil: κ1', cons: σ1' }, ElimList { nil: κ2', cons: σ2' } -> do
                     κ  <- κ1' ∨? κ2'
                     σ' <- σ1' ∨? σ2'
                     pure $ ElimList { nil: κ, cons: σ' }
                  _, _ -> Nothing
   maybeJoin _ _ = Nothing

   bot (ElimVar x κ)
      = ElimVar x (bot κ)
   bot (ElimBool { true : κ1, false : κ2 })
      = (ElimBool { true : bot κ1, false : bot κ2 })
   bot (ElimPair σ)
      = ElimPair (map bot σ)
   bot (ElimList { nil: κ, cons: σ })
      = ElimList { nil: bot κ, cons: map bot σ}

   top (ElimVar x κ)
      = ElimVar x (top κ)
   top (ElimBool { true : κ1, false : κ2 })
      = (ElimBool { true : top κ1, false : top κ2 })
   top (ElimPair σ)
      = ElimPair (map top σ)
   top (ElimList { nil: κ, cons: σ })
      = ElimList { nil: top κ, cons: map top σ}

hoistMaybe :: forall k . Elim (Maybe k) -> Maybe (Elim k)
hoistMaybe (ElimVar x (Just κ)) = Just $ ElimVar x κ
hoistMaybe (ElimBool { true: Just κ, false: Just κ' }) = Just $ ElimBool { true: κ, false: κ' }
hoistMaybe (ElimPair σ) = hoistMaybe (hoistMaybe <$> σ) >>= Just <<< ElimPair
hoistMaybe (ElimList { nil: Just κ, cons: σ }) = do
   σ' <- hoistMaybe (hoistMaybe <$> σ)
   pure $ ElimList { nil: κ, cons: σ' }
hoistMaybe _ = Nothing

data Module = Module (List (Either Def RecDefs))
