module Val where

import Prelude (bind, pure, class Eq)
import Bindings (Bindings)
import Expr (RecDefs, Elim, Expr)
import Selected
import Util (absurd, error, (≟))
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

instance rawValLattice :: Lattice RawVal where
   maybeJoin (Int x) (Int x') = do
    x'' <- x ≟ x'
    pure (Int x'')
   maybeJoin (Str s) (Str s') = do
    s''  <- s ≟ s'
    pure (Str s'')
   maybeJoin False False = do
    pure False
   maybeJoin True True = do
    pure True
   maybeJoin Nil Nil = do
    pure Nil
   maybeJoin (Cons e1 e1') (Cons e2 e2') = do
    e   <- e1  ∨? e2
    e'  <- e1' ∨? e2'
    pure (Cons e e')
   maybeJoin (Pair e1 e1') (Pair e2 e2') = do
    e   <- e1  ∨? e2
    e'  <- e1' ∨? e2'
    pure (Pair e e')
   maybeJoin _ _ = Nothing

   maybeMeet (Int x) (Int x') = do
    x'' <- x ≟ x'
    pure (Int x'')
   maybeMeet (Str s) (Str s') = do
    s''  <- s ≟ s'
    pure (Str s'')
   maybeMeet False False = do
    pure False
   maybeMeet True True = do
    pure True
   maybeMeet Nil Nil = do
    pure Nil
   maybeMeet (Cons e1 e1') (Cons e2 e2') = do
    e   <- e1  ∧? e2
    e'  <- e1' ∧? e2'
    pure (Cons e e')
   maybeMeet (Pair e1 e1') (Pair e2 e2') = do
    e   <- e1  ∧? e2
    e'  <- e1' ∧? e2'
    pure (Pair e e')
   maybeMeet _ _ = Nothing

   top (Cons e  x) = Val Top v
   bot (Val _ v) = Val Bot v

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val Bot

type Env = Bindings Val

instance valLattice :: Lattice Val where
   maybeJoin (Val α (Int x)) (Val α' (Int x')) = do
    α'' <- α ∨? α'
    x'' <- x ≟ x'
    pure (Val α'' (Int x''))
   maybeJoin (Val α (Str s)) (Val α' (Str s')) = do
    α''  <- α ∨? α'
    s''  <- s ≟ s'
    pure (Val α'' (Str s''))
   maybeJoin (Val α False) (Val α' False) = do
    α'' <- α ∨? α'
    pure (Val α'' False)
   maybeJoin (Val α True) (Val α' True) = do
    α'' <- α ∨? α'
    pure (Val α'' True)
   maybeJoin (Val α Nil) (Val α' Nil) = do
    α'' <- α ∨? α'
    pure (Val α'' Nil)
   maybeJoin (Val α (Cons e1 e1')) (Val α' (Cons e2 e2')) = do
    α'' <- α   ∨? α'
    e   <- e1  ∨? e2
    e'  <- e1' ∨? e2'
    pure (Val α'' (Cons e e'))
   maybeJoin (Val α (Pair e1 e1')) (Val α' (Pair e2 e2')) = do
    α'' <- α   ∨? α'
    e   <- e1  ∨? e2
    e'  <- e1' ∨? e2'
    pure (Val α'' (Pair e e'))
   maybeJoin _ _ = Nothing


   maybeMeet (Val α (Int x)) (Val α' (Int x')) = do
    α'' <- α ∧? α'
    x'' <- x ≟ x'
    pure (Val α'' (Int x''))
   maybeMeet (Val α (Str s)) (Val α' (Str s')) = do
    α''  <- α ∧? α'
    s''  <- s ≟ s'
    pure (Val α'' (Str s''))
   maybeMeet (Val α False) (Val α' False) = do
    α'' <- α ∧? α'
    pure (Val α'' False)
   maybeMeet (Val α True) (Val α' True) = do
    α'' <- α ∧? α'
    pure (Val α'' True)
   maybeMeet (Val α Nil) (Val α' Nil) = do
    α'' <- α ∧? α'
    pure (Val α'' Nil)
   maybeMeet (Val α (Cons e1 e1')) (Val α' (Cons e2 e2')) = do
    α'' <- α   ∧? α'
    e   <- e1  ∧? e2
    e'  <- e1' ∧? e2'
    pure (Val α'' (Cons e e'))
   maybeMeet (Val α (Pair e1 e1')) (Val α' (Pair e2 e2')) = do
    α'' <- α   ∧? α'
    e   <- e1  ∧? e2
    e'  <- e1' ∧? e2'
    pure (Val α'' (Pair e e'))
   maybeMeet _ _ = Nothing


   top (Val _ v) = Val Top v
   bot (Val _ v) = Val Bot v