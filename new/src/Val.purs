module Val where

import Prelude (bind, pure, class Eq)
import Bindings (Bindings)
import Expr (RecDefs, Elim, Expr)
import Selected (class Lattice, Selected(..), bot, join, meet, top, maybeJoin, maybeMeet, 
                 (∧?), (∧), (∨?), (∨))
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

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val Bot

type Env = Bindings Val


instance valLattice :: Lattice Val where
   maybeJoin (Val α (Int x)) (Val α' (Int x')) 
    = do 
    α'' <- α ∨? α' 
    x'' <- x ≟ x' 
    pure (Val α'' (Int x''))
   maybeJoin (Val α (Str s)) (Val α' (Str s')) 
    = do 
    α''  <- α ∨? α' 
    s''  <- s ≟ s' 
    pure (Val α'' (Str s''))
   maybeJoin (Val α False) (Val α' False) 
    = do 
    α'' <- α ∨? α' 
    pure (Val α'' False) 
   maybeJoin (Val α True) (Val α' True) 
    = do 
    α'' <- α ∨? α' 
    pure (Val α'' True) 
   maybeJoin (Val α Nil) (Val α' Nil) 
    = do 
    α'' <- α ∨? α'
    pure (Val α'' Nil)
   maybeJoin (Val α (Cons e1 e1')) (Val α' (Cons e2 e2'))
    = do 
    α'' <- α   ∨? α' 
    e   <- e1  ∨? e2
    e'  <- e1' ∨? e2'
    pure (Val α'' (Cons e e')) 
   maybeJoin (Val α (Pair e1 e1')) (Val α' (Pair e2 e2'))
    = do 
    α'' <- α   ∨? α' 
    e   <- e1  ∨? e2
    e'  <- e1' ∨? e2'
    pure (Val α'' (Pair e e'))
   
   maybeJoin _ _ = Nothing

   join e e' = case e ∨? e' of Just e'' -> e''
                               Nothing  -> error absurd
      
   maybeMeet (Val α (Int x)) (Val α' (Int x')) 
    = do 
    α'' <- α ∧? α' 
    x'' <- x ≟ x' 
    pure (Val α'' (Int x''))
   maybeMeet (Val α (Str s)) (Val α' (Str s')) 
    = do 
    α''  <- α ∧? α' 
    s''  <- s ≟ s' 
    pure (Val α'' (Str s''))
   maybeMeet (Val α False) (Val α' False) 
    = do 
    α'' <- α ∧? α' 
    pure (Val α'' False) 
   maybeMeet (Val α True) (Val α' True) 
    = do 
    α'' <- α ∧? α' 
    pure (Val α'' True) 
   maybeMeet (Val α Nil) (Val α' Nil) 
    = do 
    α'' <- α ∧? α'
    pure (Val α'' Nil)
   maybeMeet (Val α (Cons e1 e1')) (Val α' (Cons e2 e2'))
    = do 
    α'' <- α   ∧? α' 
    e   <- e1  ∧? e2
    e'  <- e1' ∧? e2'
    pure (Val α'' (Cons e e')) 
   maybeMeet (Val α (Pair e1 e1')) (Val α' (Pair e2 e2'))
    = do 
    α'' <- α   ∧? α' 
    e   <- e1  ∧? e2
    e'  <- e1' ∧? e2'
    pure (Val α'' (Pair e e'))
   
   maybeMeet _ _ = Nothing

   meet e e' = case e ∧? e' of Just e'' -> e''
                               Nothing  -> error absurd
      
   top (Val _ v) = Val Top v
   bot (Val _ v) = Val Bot v