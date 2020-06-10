module Val where

import Prelude hiding (absurd, top)
import Bindings (Bindings)
import Elim (Elim)
import Expr (RecDefs, Expr)
import Lattice (class Lattice, Selected(..), (∧?), (∨?), bot, top)
import Util (error, (≟))
import Data.Maybe (Maybe(..))

data Unary =
   IntStr (Int -> String)

data Binary =
   IntIntInt (Int -> Int -> Int) |
   IntIntBool (Int -> Int -> Boolean)

-- String arguments are "internal" names for printing, unrelated to any user-level identifiers.
data UnaryOp =
   UnaryOp String Unary |
   PartialApp BinaryOp Val

data BinaryOp = BinaryOp String Binary

data RawVal =
   True | False |
   Int Int |
   Str String |
   Closure Env RecDefs (Elim Expr) |
   Binary BinaryOp |
   Unary UnaryOp |
   Pair Val Val |
   Nil | Cons Val Val

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val FF

type Env = Bindings Val

instance rawValLattice :: Lattice RawVal where
   maybeJoin (Int x) (Int x') = x ≟ x' <#> Int
   maybeJoin (Str s) (Str s') = s ≟ s' <#> Str
   maybeJoin False False = pure False
   maybeJoin True True = pure True
   maybeJoin Nil Nil = pure Nil
   maybeJoin (Cons e1 e2) (Cons e1' e2') = do
      e <- e1 ∨? e1'
      e2' ∨? e2' <#> Cons e
   maybeJoin (Pair e1 e2) (Pair e1' e2') = do
      e <- e1 ∨? e1'
      e2 ∨? e2' <#> Pair e
   maybeJoin (Closure ρ δ σ) (Closure ρ' δ' σ') =
      error "todo"
   maybeJoin (Binary φ) (Binary φ') =
      error "todo"
   maybeJoin (Unary φ) (Unary φ') =
      error "todo"
   maybeJoin _ _ = Nothing

   maybeMeet (Int x) (Int x') = x ≟ x' <#> Int
   maybeMeet (Str s) (Str s') = s ≟ s' <#> Str
   maybeMeet False False = pure False
   maybeMeet True True = pure True
   maybeMeet Nil Nil = pure Nil
   maybeMeet (Cons e1 e2) (Cons e1' e2') = do
      e <- e1 ∨? e1'
      e2' ∧? e2' <#> Cons e
   maybeMeet (Pair e1 e2) (Pair e1' e2') = do
      e <- e1 ∨? e1'
      e2 ∧? e2' <#> Pair e
   maybeMeet (Closure ρ δ σ) (Closure ρ' δ' σ') =
      error "todo"
   maybeMeet (Binary φ) (Binary φ') =
      error "todo"
   maybeMeet (Unary φ) (Unary φ') =
      error "todo"
   maybeMeet _ _ = Nothing

   top (Int x) = Int x
   top (Str s) = Str s
   top False = False
   top True = True
   top Nil = Nil
   top (Cons e1 e2) = Cons (top e1) (top e2)
   top (Pair e1 e2) = Pair (top e1) (top e2)
   top (Closure ρ δ σ) = error "todo"
   top (Binary φ) = error "todo"
   top (Unary φ) = error "todo"

   bot (Int x) = Int x
   bot (Str s) = Str s
   bot False = False
   bot True = True
   bot Nil = Nil
   bot (Cons e1 e2) = Cons (bot e1) (bot e2)
   bot (Pair e1 e2) = Pair (bot e1) (bot e2)
   bot (Closure ρ δ σ) = error "todo"
   bot (Binary φ) = error "todo"
   bot (Unary φ) = error "todo"

instance valLattice :: Lattice Val where
   maybeJoin (Val α r) (Val α' r') = do
      α'' <- α ∨? α'
      r ∨? r' <#> Val α''

   maybeMeet (Val α r) (Val α' r') = do
      α'' <- α ∨? α'
      r ∧? r' <#> Val α''

   top (Val _ u) = Val TT $ top u
   bot (Val _ u) = Val FF $ bot u
